options(stringsAsFactors = FALSE, na.action = "na.omit")
set.seed(2024)

## ===== Dependencies =====
pkgs <- c("plm","readr","dplyr","stringr","ggplot2","tidyr","purrr","sandwich","rlang","tibble")
for(p in pkgs) if(!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({
  library(plm); library(readr); library(dplyr); library(stringr)
  library(ggplot2); library(tidyr); library(purrr); library(sandwich)
  library(rlang); library(tibble)
})
lag <- plm::lag  # be explicit

## ===== IO =====
INPUTS <- c(
  master       = "Euro_B_stability_master.csv",
  base         = "master_base.csv",
  drop_imputed = "master_drop_imputed.csv",
  real_rate    = "master_real_rate.csv"
)
ALL_DVS  <- c("npl_ratio_filled","log_bank_z_score","capital_adequacy_ratio_filled")
CGR_LAGS <- c(1, 2)  # Baseline CG1; CG2 kept as robustness

## ===== Output dirs =====
dir.create("out_models", showWarnings = FALSE, recursive = TRUE)
dir.create("out_tex",     showWarnings = FALSE, recursive = TRUE)
dir.create("out_figs",    showWarnings = FALSE, recursive = TRUE)

## (Optional) Clear stale outputs
CLEAR_OLD_OUTPUTS <- TRUE
if (CLEAR_OLD_OUTPUTS) {
  old_models <- list.files("out_models", pattern="^(fe_results_.*\\.(csv|tex)|coef_signs_.*\\.csv|fe_diag.*\\.csv)$", full.names=TRUE)
  old_tex    <- list.files("out_tex",     pattern="^(fe_results_.*\\.tex|coef_signs_.*\\.tex|fe_diag.*\\.tex)$", full.names=TRUE)
  old_figs   <- list.files("out_figs",    pattern="^FE_coeffs_.*\\.png$", full.names=TRUE)
  unlink(c(old_models, old_tex, old_figs), force = TRUE)
}

## ===== Helpers =====
stop_clean <- function(msg) stop(msg, call. = FALSE)
escape_tex <- function(x){
  x <- gsub("\\\\","\\\\textbackslash{}",x,perl=TRUE)
  gsub("_","\\\\_",x,fixed=TRUE)
}
read_panel <- function(path){
  if(!file.exists(path)) stop_clean(paste0("File not found: ", path))
  df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  if(!all(c("country","year") %in% names(df))) stop_clean(
    paste0("Missing required columns in ", path, ": ",
           paste(setdiff(c("country","year"), names(df)), collapse=", "))
  )
  df$year <- as.integer(df$year)
  df
}

# Standardize names; safely construct log z-score; ALWAYS build real_rate_cs when possible
standardize_vars <- function(df){
  nm <- names(df)
  
  if(!"npl_ratio_filled" %in% nm && "npl_ratio" %in% nm)
    df <- dplyr::rename(df, npl_ratio_filled = npl_ratio)
  
  if(!"capital_adequacy_ratio_filled" %in% nm && "capital_adequacy_ratio" %in% nm)
    df <- dplyr::rename(df, capital_adequacy_ratio_filled = capital_adequacy_ratio)
  
  if(!"log_bank_z_score" %in% nm){
    lvl_cands <- c("bank_z_score","z_score","zscore","banks_z","bank_z")
    lvl <- intersect(lvl_cands, nm)
    if(length(lvl) >= 1){
      v <- df[[lvl[1]]]
      v[!is.finite(v) | v <= 0] <- NA_real_
      df$log_bank_z_score <- log(v)
    }
  }
  
  syn <- function(target, alts){
    if(!target %in% names(df)){
      hit <- intersect(alts, names(df))
      if(length(hit)) df <- dplyr::rename(df, !!target := !!rlang::sym(hit[1]))
    }
    df
  }
  df <- syn("unemployment_rate", c("unemployment","unemp_rate","urate"))
  df <- syn("real_rate",         c("real_interest_rate","rir","real_r","realrate"))
  df <- syn("gdp_growth",        c("gdp_growth_rate","real_gdp_growth","gdp_g","rgdp_growth"))
  df <- syn("inflation",         c("inflation_rate","cpi_inflation","hicp"))
  df <- syn("policy_rate",       c("policy","mpr","refi_rate","main_refinancing_rate","mro_rate"))
  df <- syn("credit_to_gdp_gap", c("credit_gap","bis_gap","credit_to_gdp","credit_gap_bis"))
  df <- syn("credit_growth",     c("credit_g","dlog_credit","credit_gr","credit_growth_rate"))
  
  # Fallback real rate if missing and inputs exist
  if(!"real_rate" %in% names(df) && all(c("policy_rate","inflation") %in% names(df))){
    df$real_rate <- df$policy_rate - df$inflation
  }
  
  # ALWAYS build cross-sectional real rate if policy_rate & inflation exist
  if(all(c("policy_rate","inflation") %in% names(df))){
    df$real_rate_cs <- df$policy_rate - df$inflation
  }
  
  df
}

# Keep countries with at least 2 usable years for a given DV + controls
thin_country_filter_fe <- function(df, dv, base_ctrls, cg_lag = NA_integer_){
  present_ctrls <- base_ctrls[base_ctrls %in% names(df)]
  use <- unique(c("country","year", dv, present_ctrls))
  if(!is.na(cg_lag) && "credit_growth" %in% names(df)) use <- unique(c(use, "credit_growth"))
  
  d0 <- df[, intersect(use, names(df)), drop = FALSE]
  d1 <- d0[stats::complete.cases(d0), , drop = FALSE]
  
  agg <- d1 %>% dplyr::group_by(country) %>% dplyr::summarize(T = dplyr::n_distinct(year), .groups="drop")
  keep <- agg %>% dplyr::filter(T >= 2) %>% dplyr::pull(country)
  
  list(df = d1 %>% dplyr::filter(country %in% keep),
       dropped = setdiff(unique(d1$country), keep))
}

# Compose controls (prefer real_rate_cs ⇒ drop inflation to avoid collinearity with year FE)
# ---- REPLACE compose_ctrls() WITH THIS VERSION ----
compose_ctrls <- function(df, cg_lag = NA_integer_, force_rate = c("auto","real","policy")){
  force_rate <- match.arg(force_rate)
  
  # macro block: start with GDP, unemployment, inflation
  ctrl <- character(0)
  for(v in c("gdp_growth","unemployment_rate","inflation")){
    if(v %in% names(df)) ctrl <- c(ctrl, v)
  }
  
  rate_used <- NA_character_
  add <- function(v){ ctrl <<- c(ctrl, v); rate_used <<- v }
  
  if (force_rate == "policy") {
    if ("policy_rate" %in% names(df)) add("policy_rate")
  } else if (force_rate == "real") {
    if ("real_rate" %in% names(df)) add("real_rate")
    else if ("real_rate_cs" %in% names(df)) add("real_rate_cs")
    else if ("policy_rate" %in% names(df)) add("policy_rate")
  } else { # auto
    if ("real_rate" %in% names(df)) add("real_rate")
    else if ("real_rate_cs" %in% names(df)) add("real_rate_cs")
    else if ("policy_rate" %in% names(df)) add("policy_rate")
  }
  
  # >>> PATCH: avoid collinearity with year FE when using a "real" rate
  if (!is.na(rate_used) && rate_used %in% c("real_rate","real_rate_cs")) {
    ctrl <- setdiff(ctrl, "inflation")
  }
  # <<<
  
  if ("credit_to_gdp_gap" %in% names(df)) ctrl <- c(ctrl, "credit_to_gdp_gap")
  
  cg_term <- NA_character_
  if(!is.na(cg_lag) && "credit_growth" %in% names(df)){
    cg_term <- sprintf("lag(credit_growth,%d)", cg_lag)
  }
  
  list(ctrl = unique(ctrl), rate_used = rate_used, cg_term = cg_term)
}

# Fit FE and compute SEs: clustered by country & Driscoll–Kraay (explicit maxlag)
fit_fe_dual_ses <- function(pdat, fml){
  fit <- tryCatch(plm(fml, data=pdat, model="within", effect="twoways"),
                  error=function(e) e)
  if(inherits(fit,"error")){
    return(list(ok=FALSE, status=paste("fit_error:", conditionMessage(fit))))
  }
  
  # Clustered by country (baseline)
  Vc <- tryCatch(plm::vcovHC(fit, type="HC1", cluster="group"), error=function(e) NULL)
  if(is.null(Vc)) Vc <- vcov(fit)
  se_c <- sqrt(diag(Vc))
  
  # Driscoll–Kraay (robustness) with explicit maxlag
  years_unique <- tryCatch(length(unique(index(pdat)[[2]])), error=function(e) NA_integer_)
  maxlag <- if(is.na(years_unique)) 1L else max(1L, floor(4 * (years_unique^(2/9))))
  Vd <- tryCatch(plm::vcovSCC(fit, type="HC1", maxlag = maxlag), error=function(e) NULL)
  if(is.null(Vd)) Vd <- vcov(fit)
  se_d <- sqrt(diag(Vd))
  
  list(ok=TRUE, fit=fit, se_cluster=se_c, se_dk=se_d)
}

coef_table_from <- function(beta, se){
  common <- intersect(names(beta), names(se))
  b <- beta[common]; s <- se[common]
  tval <- b / s
  pval <- 2 * pnorm(abs(tval), lower.tail = FALSE)
  data.frame(term=names(b), beta=as.numeric(b), se=as.numeric(s),
             t=as.numeric(tval), p=as.numeric(pval), check.names=FALSE)
}

write_coef_outputs <- function(tab, panel, dv, spec_tag, se_tag){
  # CSV
  readr::write_csv(tab, file.path("out_models", sprintf("fe_results_%s_%s_%s_%s.csv",
                                                        panel, dv, spec_tag, se_tag)))
  # TeX
  tex <- file.path("out_tex", sprintf("fe_results_%s_%s_%s_%s.tex", panel, dv, spec_tag, se_tag))
  cat("\\scriptsize\n\\setlength{\\tabcolsep}{4.5pt}\n\\setlength\\LTleft{0pt}\n\\setlength\\LTright{0pt}\n",
      "\\begin{longtable}{lrrrr}\n\\hline\nTerm & $\\beta$ & SE & $t$ & $p$\\\\\\hline\n", file=tex)
  if(nrow(tab)){
    apply(tab, 1, function(r){
      cat(sprintf("%s & %.6f & %.6f & %.3f & %.3f\\\\\n",
                  escape_tex(as.character(r[["term"]])),
                  as.numeric(r[["beta"]]),
                  as.numeric(r[["se"]]),
                  as.numeric(r[["t"]]),
                  as.numeric(r[["p"]])),
          file=tex, append=TRUE)
    })
  }
  cat("\\hline\n\\end{longtable}\n", file=tex, append=TRUE)
  
  # Plot
  if(nrow(tab)){
    p <- ggplot(tab, aes(x=term, y=beta, ymin=beta-se, ymax=beta+se)) +
      geom_pointrange() + geom_hline(yintercept=0, linetype="dashed") +
      theme_minimal() +
      labs(title=paste("FE:", panel, dv, "—", spec_tag, "/", se_tag),
           x=NULL, y="β (±1 SE)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(filename=file.path("out_figs",
                              sprintf("FE_coeffs_%s_%s_%s_%s.png", panel, dv, spec_tag, se_tag)),
           plot=p, width=7, height=5, dpi=300)
  }
}

## ===== ONE FE ATTEMPT (no LDV in FE baseline) =====
fe_attempt <- function(df_model, panel, dv, cg_lag = NA_integer_, force_rate = c("real","policy","auto")){
  force_rate <- match.arg(force_rate)
  
  cfg <- compose_ctrls(df_model, cg_lag=cg_lag, force_rate=force_rate)
  ctrls     <- cfg$ctrl
  cg_term   <- cfg$cg_term
  rate_used <- cfg$rate_used
  
  filt <- thin_country_filter_fe(df_model, dv, base_ctrls=ctrls, cg_lag=cg_lag)
  df_sub <- filt$df; dropped <- filt$dropped
  if(nrow(df_sub) == 0){
    return(list(ok=FALSE, status=sprintf("no usable rows; dropped_countries=%d", length(dropped))))
  }
  
  pdat <- pdata.frame(df_sub, index=c("country","year"))
  
  rhs_parts <- character(0)
  if(length(ctrls)) rhs_parts <- c(rhs_parts, ctrls)
  if(!is.na(cg_term)) rhs_parts <- c(rhs_parts, cg_term)
  rhs <- paste(rhs_parts, collapse=" + ")
  fml <- as.formula(paste0(dv, " ~ ", rhs), env = environment())
  
  fitres <- fit_fe_dual_ses(pdat, fml)
  if(!isTRUE(fitres$ok)) return(list(ok=FALSE, status=fitres$status))
  
  fit  <- fitres$fit
  beta <- tryCatch(coef(fit), error=function(e) NULL)
  if(is.null(beta)) return(list(ok=FALSE, status="coef_extract_failed"))
  
  N_obs   <- tryCatch(nobs(fit), error=function(e) NA_integer_)
  n_ctry  <- length(unique(df_sub$country))
  n_years <- length(unique(df_sub$year))
  pooled  <- tryCatch(plm(fml, data=pdat, model="pooling"), error=function(e) NULL)
  pF      <- tryCatch(plm::pFtest(fit, pooled)$p.value, error=function(e) NA_real_)
  random  <- tryCatch(plm(fml, data=pdat, model="random"), error=function(e) NULL)
  haus    <- tryCatch(plm::phtest(fit, random)$p.value, error=function(e) NA_real_)
  fe_pref <- (!is.na(pF) && pF < 0.05) && (!is.na(haus) && haus < 0.05)
  
  tab_cluster <- coef_table_from(beta, fitres$se_cluster)
  tab_dk      <- coef_table_from(beta, fitres$se_dk)
  
  spec_tag <- paste0("CG", ifelse(is.na(cg_lag),"0",cg_lag), "_", ifelse(is.na(rate_used),"noRate",rate_used))
  
  write_coef_outputs(tab_cluster, panel, dv, spec_tag, "CLUSTER")
  write_coef_outputs(tab_dk,      panel, dv, spec_tag, "DK")
  
  diag_row <- function(se_type){
    data.frame(
      panel=panel, dv=dv, N=N_obs, countries=n_ctry, years=n_years,
      se_type=se_type, FE_vs_pooled_p=pF, Hausman_p=haus, FE_preferred=fe_pref,
      rate_used=ifelse(is.na(rate_used),"--",rate_used),
      cg_lag=ifelse(is.na(cg_lag),"--",as.character(cg_lag)),
      controls_used=paste(c(ctrls, if(!is.na(cg_term)) cg_term), collapse="+"),
      dropped_countries=length(dropped),
      status="ok", stringsAsFactors=FALSE
    )
  }
  
  list(ok=TRUE, diag=rbind(diag_row("CR1-group"), diag_row("DK(HC1)")))
}

run_panel_fe <- function(path, panel){
  message(">>> FE: reading panel: ", panel, " (", path, ")")
  df <- read_panel(path) %>% standardize_vars()
  
  dvs_present <- intersect(ALL_DVS, names(df))
  if(length(dvs_present)==0){
    message("No target DVs present in ", panel, " — skipping.")
    return(tibble::tibble(
      panel=panel, dv=ALL_DVS, N=NA_integer_, countries=length(unique(df$country)),
      years=NA_integer_, se_type=NA_character_,
      FE_vs_pooled_p=NA_real_, Hausman_p=NA_real_, FE_preferred=FALSE,
      rate_used=NA_character_, cg_lag=NA_character_,
      controls_used=NA_character_, dropped_countries=NA_integer_, status="dv_missing"
    ))
  }
  
  diag_rows <- list()
  
  for(dv in dvs_present){
    rate_choice <- "auto"
    if("credit_growth" %in% names(df)){
      for(k in CGR_LAGS){
        res <- fe_attempt(df, panel, dv, cg_lag=k, force_rate=rate_choice)
        if(isTRUE(res$ok)) {
          diag_rows[[length(diag_rows)+1]] <- res$diag
        } else {
          diag_rows[[length(diag_rows)+1]] <- data.frame(
            panel=panel, dv=dv, N=NA_integer_, countries=length(unique(df$country)),
            years=NA_integer_, se_type=NA_character_,
            FE_vs_pooled_p=NA_real_, Hausman_p=NA_real_, FE_preferred=FALSE,
            rate_used=rate_choice, cg_lag=as.character(k),
            controls_used=NA_character_, dropped_countries=NA_integer_,
            status=res$status, stringsAsFactors=FALSE
          )
        }
      }
    } else {
      res <- fe_attempt(df, panel, dv, cg_lag=NA_integer_, force_rate=rate_choice)
      if(isTRUE(res$ok)) {
        diag_rows[[length(diag_rows)+1]] <- res$diag
      } else {
        diag_rows[[length(diag_rows)+1]] <- data.frame(
          panel=panel, dv=dv, N=NA_integer_, countries=length(unique(df$country)),
          years=NA_integer_, se_type=NA_character_,
          FE_vs_pooled_p=NA_real_, Hausman_p=NA_real_, FE_preferred=FALSE,
          rate_used=rate_choice, cg_lag="--",
          controls_used=NA_character_, dropped_countries=NA_integer_,
          status=res$status, stringsAsFactors=FALSE
        )
      }
    }
  }
  
  dplyr::bind_rows(diag_rows)
}

## ===== Run all panels that exist =====
existing <- INPUTS[file.exists(INPUTS)]
if(length(existing)==0) stop_clean("None of the expected input CSVs are in the working directory.")
fe_diags <- purrr::imap_dfr(existing, run_panel_fe)

## ===== Save & print diagnostic summary =====
readr::write_csv(fe_diags, file.path("out_models","fe_diagnostics_summary_all.csv"))
message("\n=== FE Diagnostics Summary ===")
print(fe_diags %>%
        dplyr::select(panel, dv, N, countries, years, se_type,
                      FE_vs_pooled_p, Hausman_p, FE_preferred,
                      rate_used, cg_lag, controls_used, dropped_countries, status))

## ===== Pretty LaTeX diagnostics summary table =====
fmt_int <- function(x) ifelse(is.na(x), "--", formatC(as.integer(x), format="d"))
fmt_p   <- function(x) ifelse(is.na(x), "--", formatC(as.numeric(x), digits=3, format="f"))
fmt_txt <- function(x) ifelse(is.na(x) | x=="", "--", escape_tex(as.character(x)))
tex_diag <- file.path("out_tex","fe_diagnostics_summary_all.tex")
con <- file(tex_diag, "w")
cat("\\scriptsize\n\\setlength{\\tabcolsep}{4.5pt}\n\\setlength\\LTleft{0pt}\n\\setlength\\LTright{0pt}\n",
    "\\begin{longtable}{l l r r r l r r c c l r l}\n",
    "\\caption{Two-way FE diagnostics across panels, DVs and specs}\\label{tab:fe_diags}\\\\\n",
    "\\hline\n",
    "Panel & DV & N & Ctries & Years & SEs & pF (FE vs pooled) & Hausman & FE pref & Rate & CG lag & Controls & Dropped\\\\\n",
    "\\hline\n\\endfirsthead\n",
    "\\hline\n",
    "Panel & DV & N & Ctries & Years & SEs & pF (FE vs pooled) & Hausman & FE pref & Rate & CG lag & Controls & Dropped\\\\\n",
    "\\hline\n\\endhead\n",
    file = con)
for (i in seq_len(nrow(fe_diags))) {
  r <- fe_diags[i, ]
  cat(sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s\\\\\n",
              fmt_txt(r$panel),
              fmt_txt(r$dv),
              fmt_int(r$N),
              fmt_int(r$countries),
              fmt_int(r$years),
              fmt_txt(r$se_type),
              fmt_p(r$FE_vs_pooled_p),
              fmt_p(r$Hausman_p),
              ifelse(isTRUE(r$FE_preferred), "Yes", "No"),
              fmt_txt(r$rate_used),
              fmt_txt(r$cg_lag),
              fmt_txt(r$controls_used),
              fmt_int(r$dropped_countries)),
      file = con)
}
cat("\\hline\n\\end{longtable}\n", file = con)
close(con)
message("Wrote: ", tex_diag)

## ===== FE-only sign summary (prefer CLUSTER, CG1 only) =====
star <- function(p) ifelse(is.na(p),"", ifelse(p<.01,"***", ifelse(p<.05,"**", ifelse(p<.10,"*",""))))
fe_files_all <- list.files("out_models",
                           pattern="^fe_results_(.+?)_(.+?)_(CG\\d+_.+?)_(CLUSTER|DK)\\.csv$",
                           full.names = TRUE)
fe_files <- fe_files_all[grepl("_CG1_", fe_files_all) | !grepl("_CG\\d+_", fe_files_all)]
fe_tbl <- purrr::map_dfr(fe_files, function(f){
  m <- stringr::str_match(basename(f), "^fe_results_(.+?)_(.+?)_(CG\\d+_.+?)_(CLUSTER|DK)\\.csv$")
  panel <- m[2]; dv <- m[3]; spec <- m[4]; se_type <- m[5]
  readr::read_csv(f, show_col_types = FALSE) %>%
    dplyr::mutate(panel=panel, dv=dv, method=paste0("FE(",se_type,")"), spec=spec,
                  sign=dplyr::case_when(beta > 0 ~ "+", beta < 0 ~ "−", TRUE ~ "0"),
                  stars=star(p)) %>%
    dplyr::select(panel, dv, method, spec, term, beta, se, p, stars, sign)
})

fe_tbl_pref <- fe_tbl %>%
  dplyr::mutate(pref = ifelse(grepl("^FE\\(CLUSTER\\)", method), 1L, 2L)) %>%
  dplyr::group_by(panel, dv, term) %>%
  dplyr::arrange(pref, .by_group=TRUE) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-pref)

key_terms <- c("gdp_growth","unemployment_rate","inflation","policy_rate","real_rate","real_rate_cs","credit_to_gdp_gap")
sign_summary <- fe_tbl_pref %>%
  dplyr::filter(term %in% key_terms) %>%
  dplyr::arrange(panel, dv, method, term)

out_sign_csv  <- file.path("out_models","coef_signs_summary_FE_only.csv")
readr::write_csv(sign_summary, out_sign_csv)

tex_signs <- file.path("out_tex","coef_signs_summary_FE_only.tex")
cat("\\scriptsize\n\\setlength{\\tabcolsep}{4.5pt}\n\\setlength\\LTleft{0pt}\n\\setlength\\LTright{0pt}\n",
    "\\begin{longtable}{l l l l c c r r r}\n\\hline\n",
    "Panel & DV & Method & Term & Sign & Stars & $\\beta$ & SE & $p$ \\\\\\hline\n", file=tex_signs)
apply(sign_summary, 1, function(r){
  cat(sprintf("%s & %s & %s & %s & %s & %s & %.6f & %.6f & %.3f\\\\\n",
              r[["panel"]], r[["dv"]], r[["method"]],
              gsub("_","\\_", r[["term"]], fixed=TRUE),
              r[["sign"]], r[["stars"]],
              as.numeric(r[["beta"]]), as.numeric(r[["se"]]), as.numeric(r[["p"]])),
      file=tex_signs, append=TRUE)
})
cat("\\hline\n\\end{longtable}\n", file=tex_signs, append=TRUE)
message("Wrote: ", out_sign_csv, " and ", tex_signs)

## ===== Diagnostics + representative FE(CLUSTER) signs per (panel,dv), CG1 baseline =====
sgn  <- function(b) ifelse(is.na(b),"", ifelse(b>0,"+", ifelse(b<0,"−","0")))
prefer_real <- function(files){
  if(length(files) == 0) return(files)
  fbase <- basename(files)
  c(files[grepl("_real_rate", fbase)], files[!grepl("_real_rate", fbase)])
}
grab_rep_file <- function(panel, dv){
  files_cl1 <- list.files("out_models",
                          pattern=paste0("^fe_results_", panel, "_", dv, "_(CG1_.+?)_CLUSTER\\.csv$"),
                          full.names = TRUE)
  files_cl1 <- prefer_real(files_cl1)
  if(length(files_cl1) > 0) return(files_cl1[1])
  files_dk1 <- list.files("out_models",
                          pattern=paste0("^fe_results_", panel, "_", dv, "_(CG1_.+?)_DK\\.csv$"),
                          full.names = TRUE)
  files_dk1 <- prefer_real(files_dk1)
  if(length(files_dk1) > 0) return(files_dk1[1])
  files_cl <- list.files("out_models",
                         pattern=paste0("^fe_results_", panel, "_", dv, "_(CG\\d+_.+?)_CLUSTER\\.csv$"),
                         full.names = TRUE)
  if(length(files_cl) > 0){
    df <- tibble(path = files_cl) %>%
      dplyr::mutate(spec = stringr::str_match(basename(path), "_(CG\\d+_.+?)_CLUSTER\\.csv$")[,2],
                    cgnum = as.integer(stringr::str_match(spec, "^CG(\\d+)")[,2]),
                    real  = grepl("_real_rate", basename(path))) %>%
      dplyr::arrange(cgnum, dplyr::desc(real))
    return(df$path[1])
  }
  files_dk <- list.files("out_models",
                         pattern=paste0("^fe_results_", panel, "_", dv, "_(CG\\d+_.+?)_DK\\.csv$"),
                         full.names = TRUE)
  if(length(files_dk) > 0){
    df <- tibble(path = files_dk) %>%
      dplyr::mutate(spec = stringr::str_match(basename(path), "_(CG\\d+_.+?)_DK\\.csv$")[,2],
                    cgnum = as.integer(stringr::str_match(spec, "^CG(\\d+)")[,2]),
                    real  = grepl("_real_rate", basename(path))) %>%
      dplyr::arrange(cgnum, dplyr::desc(real))
    return(df$path[1])
  }
  NA_character_
}
extract_rep_signs <- function(panel, dv){
  f <- grab_rep_file(panel, dv)
  if(is.na(f) || !file.exists(f)) {
    return(tibble(
      panel=panel, dv=dv,
      FErep_spec="--", FErep_se="--",
      FErep_sign_gdp="", FErep_star_gdp="",
      FErep_sign_unemp="", FErep_star_unemp="",
      FErep_sign_infl="", FErep_star_infl="",
      FErep_rate_term="--", FErep_sign_rate="", FErep_star_rate="",
      FErep_sign_gap="", FErep_star_gap="",
      FErep_sign_lagcg="", FErep_star_lagcg=""
    ))
  }
  cf <- readr::read_csv(f, show_col_types = FALSE)
  
  cand <- intersect(c("real_rate_cs","real_rate","policy_rate"), cf$term)
  if (length(cand)) {
    rate_term <- cand[1]
  } else if (any(grepl("(^|_)real[_ ]?rate(_cs)?($|_)", cf$term))) {
    rate_term <- cf$term[grepl("(^|_)real[_ ]?rate(_cs)?($|_)", cf$term)][1]
  } else if (any(grepl("(^|_)policy[_ ]?rate($|_)", cf$term))) {
    rate_term <- cf$term[grepl("(^|_)policy[_ ]?rate($|_)", cf$term)][1]
  } else {
    rate_term <- NA_character_
  }
  
  lagcg_term <- if(any(grepl("^lag\\(credit_growth,\\s*\\d+\\)$", cf$term))) {
    cf$term[grepl("^lag\\(credit_growth,\\s*\\d+\\)$", cf$term)][1]
  } else NA_character_
  
  get <- function(term){
    if(is.na(term)) return(c(sign="", stars=""))
    row <- cf %>% dplyr::filter(term == !!term) %>% dplyr::slice(1)
    if(nrow(row)==0) return(c(sign="", stars=""))
    c(sign = sgn(row$beta), stars = star(row$p))
  }
  
  tibble(
    panel=panel, dv=dv,
    FErep_spec = stringr::str_match(basename(f), "_(CG\\d+_.+?)_(DK|CLUSTER)\\.csv$")[,2],
    FErep_se   = stringr::str_match(basename(f), "_(DK|CLUSTER)\\.csv$")[,2],
    FErep_sign_gdp   = get("gdp_growth")["sign"], FErep_star_gdp = get("gdp_growth")["stars"],
    FErep_sign_unemp = get("unemployment_rate")["sign"], FErep_star_unemp = get("unemployment_rate")["stars"],
    FErep_sign_infl  = get("inflation")["sign"], FErep_star_infl = get("inflation")["stars"],
    FErep_rate_term  = ifelse(is.na(rate_term),"--",rate_term),
    FErep_sign_rate  = get(rate_term)["sign"],  FErep_star_rate  = get(rate_term)["stars"],
    FErep_sign_gap   = get("credit_to_gdp_gap")["sign"], FErep_star_gap = get("credit_to_gdp_gap")["stars"],
    FErep_sign_lagcg = get(lagcg_term)["sign"], FErep_star_lagcg = get(lagcg_term)["stars"]
  )
}

rep_signs <- fe_diags %>%
  dplyr::distinct(panel, dv) %>%
  dplyr::arrange(panel, dv) %>%
  purrr::pmap_dfr(~extract_rep_signs(..1, ..2))

fe_diags_plus <- fe_diags %>%
  dplyr::left_join(rep_signs, by=c("panel","dv")) %>%
  dplyr::mutate(
    GDP    = paste0(FErep_sign_gdp, FErep_star_gdp),
    UNEMP  = paste0(FErep_sign_unemp, FErep_star_unemp),
    INFL   = paste0(FErep_sign_infl, FErep_star_infl),
    has_rate_in_controls = grepl("(real_rate_cs|real_rate|policy_rate)", controls_used),
    RATE   = dplyr::case_when(
      FErep_rate_term == "--" & has_rate_in_controls ~ "absorbed by year FE",
      FErep_rate_term == "--" ~ "--",
      TRUE ~ paste0(FErep_rate_term, ":", FErep_sign_rate, FErep_star_rate)
    ),
    GAP    = paste0(FErep_sign_gap, FErep_star_gap),
    L_CG   = dplyr::if_else(FErep_sign_lagcg=="", "--", paste0(FErep_sign_lagcg, FErep_star_lagcg)),
    FErep_choice = paste0(FErep_spec, " / ", FErep_se)
  )

out_diag_plus_csv <- file.path("out_models","fe_diagnostics_plus_signs.csv")
readr::write_csv(fe_diags_plus, out_diag_plus_csv)

{
  op <- options(na.print = "NA")
  on.exit(options(op), add = TRUE)
  fe_diags_plus %>%
    dplyr::select(panel, dv, se_type, FE_vs_pooled_p, Hausman_p, FE_preferred,
                  rate_used, cg_lag, controls_used, GDP, UNEMP, INFL, RATE, GAP, L_CG, FErep_choice) %>%
    dplyr::arrange(panel, dv, se_type) %>%
    as.data.frame() %>%
    print(row.names = FALSE)
}

out_diag_plus_tex <- file.path("out_tex","fe_diagnostics_plus_signs.tex")
con2 <- file(out_diag_plus_tex, "w")
cat("\\scriptsize\n\\setlength{\\tabcolsep}{4.5pt}\n\\setlength\\LTleft{0pt}\n\\setlength\\LTright{0pt}\n",
    "\\begin{longtable}{l l r r r l r r c l l l l l l l l}\n",
    "\\caption{Two-way FE diagnostics with baseline (CLUSTER, CG1) coefficient signs}\\label{tab:fe_diags_plus}\\\\\n",
    "\\hline\n",
    "Panel & DV & N & Ctries & Years & SEs & pF (FE vs pooled) & Hausman & FE pref & Rate & CG lag & GDP & UNEMP & INFL & RATE & GAP & L.CG \\\\\\hline\n",
    "\\endfirsthead\n\\hline\n",
    "Panel & DV & N & Ctries & Years & SEs & pF (FE vs pooled) & Hausman & FE pref & Rate & CG lag & GDP & UNEMP & INFL & RATE & GAP & L.CG \\\\\\hline\n",
    "\\endhead\n", file = con2)
for(i in seq_len(nrow(fe_diags_plus))){
  r <- fe_diags_plus[i,]
  cat(sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s & %s\\\\\n",
              fmt_txt(r$panel),
              fmt_txt(r$dv),
              fmt_int(r$N),
              fmt_int(r$countries),
              fmt_int(r$years),
              fmt_txt(r$se_type),
              fmt_p(r$FE_vs_pooled_p),
              fmt_p(r$Hausman_p),
              ifelse(isTRUE(r$FE_preferred),"Yes","No"),
              fmt_txt(r$rate_used),
              fmt_txt(r$cg_lag),
              fmt_txt(r$GDP),
              fmt_txt(r$UNEMP),
              fmt_txt(r$INFL),
              fmt_txt(r$RATE),
              fmt_txt(r$GAP),
              fmt_txt(r$L_CG)),
      file = con2)
}
cat("\\hline\n\\end{longtable}\n", file = con2)
close(con2)

message("Wrote:\n  - ", file.path("out_models","coef_signs_summary_FE_only.csv"),
        "\n  - ", file.path("out_tex","coef_signs_summary_FE_only.tex"),
        "\n  - ", out_diag_plus_csv,
        "\n  - ", out_diag_plus_tex,
        "\n  - ", file.path("out_models","fe_diagnostics_summary_all.csv"),
        "\n  - ", tex_diag,
        "\nAnd coefficient PNGs in out_figs/")

## =======================================================================================

# collect_coefficients.R — gather FE & GMM coefficients and show signs (CLUSTER-preferred)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(tidyr); library(purrr)
})

dir.create("out_tex", showWarnings = FALSE, recursive = TRUE)

star <- function(p) ifelse(is.na(p),"",
                           ifelse(p<.01,"***", ifelse(p<.05,"**", ifelse(p<.10,"*",""))))

# ---- System-GMM ----
gmm_files <- list.files("out_models", pattern="^sysgmm_results_(.+?)_(.+?)\\.csv$", full.names = TRUE)
gmm_tbl <- map_dfr(gmm_files, function(f){
  m <- str_match(basename(f), "^sysgmm_results_(.+?)_(.+?)\\.csv$")
  panel <- m[2]; dv <- m[3]
  readr::read_csv(f, show_col_types = FALSE) %>%
    mutate(panel=panel, dv=dv, method="System-GMM",
           sign=case_when(beta > 0 ~ "+", beta < 0 ~ "−", TRUE ~ "0"),
           stars=star(p)) %>%
    select(panel, dv, method, term, beta, se, p, stars, sign)
})

# ---- FE (prefer CLUSTER for consistency) ----
fe_files <- list.files("out_models", pattern="^fe_results_(.+?)_(.+?)_(CG\\d+_.+?)_(CLUSTER|DK)\\.csv$", full.names = TRUE)
fe_tbl <- map_dfr(fe_files, function(f){
  m <- str_match(basename(f), "^fe_results_(.+?)_(.+?)_(CG\\d+_.+?)_(CLUSTER|DK)\\.csv$")
  panel <- m[2]; dv <- m[3]; spec <- m[4]; se_type <- m[5]
  readr::read_csv(f, show_col_types = FALSE) %>%
    mutate(panel=panel, dv=dv, method=paste0("FE(",se_type,")"), spec=spec,
           sign=case_when(beta > 0 ~ "+", beta < 0 ~ "−", TRUE ~ "0"),
           stars=star(p)) %>%
    select(panel, dv, method, spec, term, beta, se, p, stars, sign)
})

fe_tbl_pref <- fe_tbl %>%
  mutate(pref = ifelse(grepl("^FE\\(CLUSTER\\)", method), 1L, 2L)) %>%
  group_by(panel, dv, term) %>%
  arrange(pref, .by_group=TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(-pref)

# ---- Combine and print tidy sign summary for key terms ----
key_terms <- c("lag(npl_ratio_filled, 1)","lag(log_bank_z_score, 1)","lag(capital_adequacy_ratio_filled, 1)",
               "gdp_growth","unemployment_rate","inflation","real_rate","real_rate_cs","policy_rate")

sign_summary <- bind_rows(
  gmm_tbl %>% mutate(spec=NA_character_) %>% select(panel,dv,method,spec,term,sign,stars,beta,se,p),
  fe_tbl_pref
) %>%
  filter(term %in% key_terms) %>%
  arrange(panel, dv, method, term)

print(sign_summary %>% select(panel, dv, method, term, sign, stars, beta, se, p), n=200)

# Save CSV
readr::write_csv(sign_summary, file.path("out_models","coef_signs_summary_FE_and_GMM.csv"))

# Save LaTeX longtable
tex <- file.path("out_tex","coef_signs_summary_FE_and_GMM.tex")
cat("\\scriptsize\n\\setlength{\\tabcolsep}{4.5pt}\n\\setlength\\LTleft{0pt}\n\\setlength\\LTright{0pt}\n",
    "\\begin{longtable}{l l l l c c r r r}\n\\hline\n",
    "Panel & DV & Method & Term & Sign & Stars & $\\beta$ & SE & $p$ \\\\\\hline\n", file=tex)
apply(sign_summary, 1, function(r){
  cat(sprintf("%s & %s & %s & %s & %s & %s & %.6f & %.6f & %.3f\\\\\n",
              r[["panel"]], r[["dv"]], r[["method"]],
              gsub("_","\\_", r[["term"]], fixed=TRUE),
              r[["sign"]], r[["stars"]],
              as.numeric(r[["beta"]]), as.numeric(r[["se"]]), as.numeric(r[["p"]])),
      file=tex, append=TRUE)
})
cat("\\hline\n\\end{longtable}\n", file=tex, append=TRUE)

message("\nWrote: out_models/coef_signs_summary_FE_and_GMM.csv and out_tex/coef_signs_summary_FE_and_GMM.tex")

