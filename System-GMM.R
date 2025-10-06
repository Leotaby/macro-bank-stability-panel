options(stringsAsFactors = FALSE)
set.seed(2024)

# ===== Dependencies =====
pkgs <- c("plm","readr","dplyr","stringr","broom","ggplot2","tidyr","purrr")
for(p in pkgs) if(!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({
  library(plm); library(readr); library(dplyr); library(stringr)
  library(broom); library(ggplot2); library(tidyr); library(purrr)
})
lag <- plm::lag  # ensure plm::lag in formulas

# ===== IO =====
INPUTS <- c(
  master       = "Euro_B_stability_master.csv",
  base         = "master_base.csv",
  drop_imputed = "master_drop_imputed.csv",
  real_rate    = "master_real_rate.csv"
)
ALL_DVS   <- c("npl_ratio_filled","log_bank_z_score","capital_adequacy_ratio_filled")
ALL_CTRL  <- c("gdp_growth","unemployment_rate","inflation","real_rate")
TIME_STRATS <- c("full","bin2","trend")  # try in this order

dir.create("out_models", showWarnings = FALSE, recursive = TRUE)
dir.create("out_tex",     showWarnings = FALSE, recursive = TRUE)
dir.create("out_figs",    showWarnings = FALSE, recursive = TRUE)

# ===== Helpers =====
stop_clean <- function(msg) stop(msg, call. = FALSE)

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

# Standardize names / construct log z-score if needed
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
  if(!"unemployment_rate" %in% nm){
    alt <- intersect(c("unemployment","unemp_rate","urate"), nm)
    if(length(alt)) df <- dplyr::rename(df, unemployment_rate = .data[[alt[1]]])
  }
  if(!"real_rate" %in% nm){
    alt <- intersect(c("real_interest_rate","rir","real_r"), nm)
    if(length(alt)) df <- dplyr::rename(df, real_rate = .data[[alt[1]]])
  }
  if(!"gdp_growth" %in% nm){
    alt <- intersect(c("gdp_growth_rate","real_gdp_growth","gdp_g"), nm)
    if(length(alt)) df <- dplyr::rename(df, gdp_growth = .data[[alt[1]]])
  }
  if(!"inflation" %in% nm){
    alt <- intersect(c("inflation_rate","cpi_inflation","hicp"), nm)
    if(length(alt)) df <- dplyr::rename(df, inflation = .data[[alt[1]]])
  }
  df
}

escape_tex <- function(x){
  x <- gsub("\\\\","\\\\textbackslash{}",x,perl=TRUE)
  gsub("_","\\\\_",x,fixed=TRUE)
}

# Instrument counter robust to plm versions
count_instruments <- function(fit){
  ni <- tryCatch({ W <- fit$W; if(!is.null(W)) as.integer(ncol(W)) else NA_integer_ }, error=function(e) NA_integer_)
  if(!is.na(ni)) return(ni)
  ni <- tryCatch({ s <- summary(fit, robust=TRUE); if(!is.null(s$k)) as.integer(s$k) else NA_integer_ }, error=function(e) NA_integer_)
  if(!is.na(ni)) return(ni)
  ni <- tryCatch({
    sg <- sargan(fit, robust=TRUE)
    df_overid <- if(!is.null(sg$parameter)) as.numeric(sg$parameter) else as.numeric(attr(sg,"parameter"))
    k_params  <- length(coef(fit))
    if(is.na(df_overid) || is.na(k_params)) NA_integer_ else as.integer(round(df_overid + k_params))
  }, error=function(e) NA_integer_)
  if(!is.na(ni)) return(ni)
  ni <- tryCatch({ V <- vcov(fit); if(is.matrix(V) && nrow(V)==ncol(V) && nrow(V)>0) as.integer(nrow(V)) else NA_integer_ },
                 error=function(e) NA_integer_)
  ni
}

thin_country_filter <- function(df_model, dv, ctrls){
  use <- unique(c("country","year", dv, ctrls))
  present <- intersect(use, names(df_model))
  d0 <- df_model[, present, drop = FALSE]
  d1 <- d0[stats::complete.cases(d0), , drop = FALSE]
  agg <- d1 %>% group_by(country) %>% summarize(T = dplyr::n_distinct(year), .groups="drop")
  keep <- agg %>% filter(T >= 4) %>% pull(country)
  list(df = d1 %>% filter(country %in% keep),
       dropped = setdiff(unique(d1$country), keep))
}

# Time IV strategy (returns both a description and a key)
build_time_iv <- function(df_sub, strategy){
  yr <- df_sub$year
  if(strategy == "full"){
    df_sub$time_iv <- factor(yr)
    iv_cols <- nlevels(df_sub$time_iv) - 1L
    return(list(df=df_sub, iv="time_iv", iv_cols=iv_cols, desc="factor(year)", key="full"))
  } else if(strategy == "bin2"){
    br <- seq(min(yr, na.rm=TRUE), max(yr, na.rm=TRUE)+2, by=2)
    df_sub$time_iv <- cut(yr, breaks=br, right=FALSE, include.lowest=TRUE)
    iv_cols <- nlevels(df_sub$time_iv) - 1L
    return(list(df=df_sub, iv="time_iv", iv_cols=iv_cols, desc="2-year bins", key="bin2"))
  } else {
    df_sub$time_trend <- as.numeric(scale(yr))
    return(list(df=df_sub, iv="time_trend", iv_cols=1L, desc="linear trend", key="trend"))
  }
}

# Handy extractor for Hansen pieces (stat, df, p)
get_hansen_triplet <- function(fit){
  h <- tryCatch(sargan(fit, robust=TRUE), error=function(e) NULL)
  if(is.null(h)) return(c(stat=NA_real_, df=NA_real_, p=NA_real_))
  stat <- suppressWarnings(as.numeric(h$statistic))
  df   <- if(!is.null(h$parameter)) suppressWarnings(as.numeric(h$parameter)) else suppressWarnings(as.numeric(attr(h,"parameter")))
  p    <- suppressWarnings(as.numeric(h$p.value))
  c(stat=stat, df=df, p=p)
}

attempt_pgmm <- function(df_model, panel, dv, L, U, tf, ctrls_available, time_strategy){
  ctrls <- intersect(ctrls_available, names(df_model))
  filt <- thin_country_filter(df_model, dv, ctrls)
  df_sub <- filt$df; dropped <- filt$dropped
  if(nrow(df_sub)==0){
    return(list(ok=FALSE, status=sprintf("no usable rows after NA drop; dropped_thin_countries=%d", length(dropped)),
                diag=NULL, fit=NULL))
  }
  
  tb <- build_time_iv(df_sub, time_strategy)
  df_sub <- tb$df; time_iv <- tb$iv; time_iv_cols <- tb$iv_cols; time_desc <- tb$desc; time_key <- tb$key
  
  pdat <- pdata.frame(df_sub, index=c("country","year"))
  
  rhs_ctrl <- if(length(ctrls)) paste(ctrls, collapse=" + ") else NULL
  rhs <- paste0("lag(", dv, ", 1)",
                if(!is.null(rhs_ctrl)) paste0(" + ", rhs_ctrl) else "",
                " + ", time_iv)  # time effects IN the regression
  
  # LDV instruments (collapsed) as before
  inst_gmm <- sprintf("lag(%s, %d:%d)", dv, L, U)
  
  # ===== CHANGED: macro controls are PREDETERMINED → use lagged (2:3) instruments =====
  inst_pre <- if (length(ctrls)) paste(sprintf("lag(%s, 2:3)", ctrls), collapse=" + ") else NULL
  
  # Keep time effects in the IV block (as in the thesis)
  inst_iv  <- paste(c(inst_pre, time_iv), collapse=" + ")
  
  fml_str  <- paste(dv, "~", rhs, "|", paste(inst_gmm, inst_iv, sep=" + "))
  fml <- as.formula(fml_str, env = environment())
  
  fit <- tryCatch(pgmm(formula=fml, data=pdat, effect="individual",
                       model="twosteps", transformation=tf, collapse=TRUE),
                  error=function(e) e)
  if(inherits(fit,"error")){
    status <- sprintf("error(L%d:%d,%s,%s): %s", L, U, tf, time_strategy, conditionMessage(fit))
    return(list(ok=FALSE, status=status, diag=NULL, fit=NULL,
                dropped=dropped, L=L, U=U, tf=tf, ctrls_used=ctrls, time_fe=time_desc, time_strategy=time_key))
  }
  
  s        <- summary(fit, robust=TRUE)
  ar1      <- tryCatch(mtest(fit, order=1)$p.value, error=function(e) NA_real_)
  ar2      <- tryCatch(mtest(fit, order=2)$p.value, error=function(e) NA_real_)
  get_hansen_triplet <- function(fit){
    h <- tryCatch(sargan(fit, robust=TRUE), error=function(e) NULL)
    if(is.null(h)) return(c(stat=NA_real_, df=NA_real_, p=NA_real_))
    c(stat=suppressWarnings(as.numeric(h$statistic)),
      df  =suppressWarnings(as.numeric(if(!is.null(h$parameter)) h$parameter else attr(h,"parameter"))),
      p   =suppressWarnings(as.numeric(h$p.value)))
  }
  hans_trip <- get_hansen_triplet(fit)
  hansen    <- hans_trip[["p"]]
  hans_stat <- hans_trip[["stat"]]
  hans_df   <- hans_trip[["df"]]
  
  n_cty   <- length(unique(index(pdat)[[1]]))
  count_instruments <- function(fit){
    ni <- tryCatch({ W <- fit$W; if(!is.null(W)) as.integer(ncol(W)) else NA_integer_ }, error=function(e) NA_integer_)
    if(!is.na(ni)) return(ni)
    ni <- tryCatch({ s <- summary(fit, robust=TRUE); if(!is.null(s$k)) as.integer(s$k) else NA_integer_ }, error=function(e) NA_integer_)
    if(!is.na(ni)) return(ni)
    ni <- tryCatch({
      sg <- sargan(fit, robust=TRUE)
      df_overid <- if(!is.null(sg$parameter)) as.numeric(sg$parameter) else as.numeric(attr(sg,"parameter"))
      k_params  <- length(coef(fit))
      if(is.na(df_overid) || is.na(k_params)) NA_integer_ else as.integer(round(df_overid + k_params))
    }, error=function(e) NA_integer_)
    if(!is.na(ni)) return(ni)
    ni <- tryCatch({ V <- vcov(fit); if(is.matrix(V) && nrow(V)==ncol(V) && nrow(V)>0) as.integer(nrow(V)) else NA_integer_ },
                   error=function(e) NA_integer_)
    ni
  }
  n_inst <- tryCatch(count_instruments(fit), error=function(e) NA_integer_)
  if(is.na(n_inst)){ # conservative fallback
    gmm_cols <- (U - L + 1) * ifelse(tf == "ld", 2, 1)
    n_inst <- as.integer(length(ctrls) + time_iv_cols + gmm_cols)
  }
  N_obs <- tryCatch(nobs(fit), error=function(e) NA_integer_)
  
  ar2_ok  <- (!is.na(ar2) && ar2 > 0.10)
  hans_ok <- (!is.na(hansen) && hansen >= 0.10 && hansen <= 0.90)
  
  # ===== CHANGED: panel-agnostic instrument cap =====
  inst_ok <- (is.na(n_inst) || n_inst < n_cty)
  
  pass <- (ar2_ok && hans_ok && inst_ok)  # AR(1) can be significant; not a guardrail
  
  coef_tab <- as.data.frame(s$coefficients)
  if(nrow(coef_tab)){
    names(coef_tab) <- c("beta","se","t","p"); coef_tab$term <- rownames(s$coefficients)
    coef_tab <- coef_tab %>% dplyr::select(term,beta,se,t,p)
  } else {
    coef_tab <- tibble::tibble(term=character(), beta=double(), se=double(), t=double(), p=double())
  }
  
  status <- if(pass){
    sprintf("ok; dropped_thin_countries=%d", length(dropped))
  } else {
    sprintf("guardrail_fail(L%d:%d,%s,%s): AR2=%.3f, Hansen=%.3f, n_inst=%s, n_cty=%d; dropped_thin_countries=%d",
            L, U, tf, time_strategy,
            ifelse(is.na(ar2), NA_real_, ar2),
            ifelse(is.na(hansen), NA_real_, hansen),
            ifelse(is.na(n_inst), "NA", as.character(n_inst)), n_cty, length(dropped))
  }
  
  diag <- data.frame(
    panel=panel, dv=dv, N=N_obs, countries=n_cty,
    n_instruments=n_inst, AR1_p=ar1, AR2_p=ar2, Hansen_p=hansen,
    Hansen_stat=hans_stat, Hansen_df=hans_df,
    DiffH_stat=NA_real_, DiffH_df=NA_real_, DiffH_p=NA_real_,  # filled later if system chosen
    passes=pass, status=status, transformation=tf, lags_used=sprintf("%d:%d",L,U),
    controls_used=paste(ctrls, collapse="+"), time_fe=time_desc, time_strategy=time_key,
    stringsAsFactors=FALSE
  )
  
  list(ok=pass, status=status, diag=diag, fit=fit,
       coef_tab=coef_tab, dropped=dropped, L=L, U=U, tf=tf,
       ctrls_used=ctrls, time_fe=time_desc, time_strategy=time_key)
}

write_outputs_if_pass <- function(res, panel, dv){
  if(!isTRUE(res$ok)) return(invisible(NULL))
  # e.g. "trend_L2U2_ld"
  spec_tag <- sprintf("%s_L%dU%d_%s", res$time_strategy, res$L, res$U, res$tf)
  
  # CSV + TeX filenames carry the spec tag
  readr::write_csv(
    res$coef_tab,
    file.path("out_models", sprintf("sysgmm_results_%s_%s_%s.csv", panel, dv, spec_tag))
  )
  
  tex <- file.path("out_tex", sprintf("sysgmm_results_%s_%s_%s.tex", panel, dv, spec_tag))
  cat("\\scriptsize\n\\setlength{\\tabcolsep}{4.5pt}\n\\setlength\\LTleft{0pt}\n\\setlength\\LTright{0pt}\n",
      "\\begin{longtable}{lrrrr}\n\\hline\nTerm & $\\beta$ & SE & $t$ & $p$\\\\\\hline\n", file=tex)
  if(nrow(res$coef_tab)){
    apply(res$coef_tab, 1, function(r){
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
}

plot_and_save <- function(diag_df, panel){
  if(!nrow(diag_df)) return(invisible(NULL))
  dd  <- diag_df %>% dplyr::filter(!is.na(AR2_p) & !is.na(Hansen_p))
  dd1 <- diag_df %>% dplyr::filter(!is.na(AR1_p))
  
  p0 <- ggplot(dd1, aes(x=dv, y=AR1_p)) + geom_col(alpha=.7) +
    geom_hline(yintercept=.10, linetype="dashed") +
    theme_minimal() + labs(title=paste("AR(1) p-values —", panel), x="DV", y="p-value") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  ggsave(filename=file.path("out_figs", sprintf("%s_AR1.png", panel)), plot=p0, width=7, height=5, dpi=300)
  
  p1 <- ggplot(dd, aes(x=dv, y=AR2_p)) + geom_col(alpha=.7) +
    geom_hline(yintercept=.10, linetype="dashed") +
    theme_minimal() + labs(title=paste("AR(2) p-values —", panel), x="DV", y="p-value") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  ggsave(filename=file.path("out_figs", sprintf("%s_AR2.png", panel)), plot=p1, width=7, height=5, dpi=300)
  
  p2 <- ggplot(dd, aes(x=dv, y=Hansen_p)) + geom_col(alpha=.7) +
    geom_hline(yintercept=.10, linetype="dashed") + geom_hline(yintercept=.90, linetype="dotted") +
    theme_minimal() + labs(title=paste("Hansen p-values —", panel), x="DV", y="p-value") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  ggsave(filename=file.path("out_figs", sprintf("%s_Hansen.png", panel)), plot=p2, width=7, height=5, dpi=300)
}

# Compute DIH for extra System moments: needs a matched "d" spec (same lags & same time_strategy)
compute_diff_hansen <- function(res_sys, res_diff){
  trip_sys  <- get_hansen_triplet(res_sys$fit)
  trip_diff <- get_hansen_triplet(res_diff$fit)
  if(any(is.na(c(trip_sys["stat"],trip_sys["df"],trip_diff["stat"],trip_diff["df"])))) {
    return(c(DiffH_stat=NA_real_, DiffH_df=NA_real_, DiffH_p=NA_real_))
  }
  dstat <- as.numeric(trip_sys["stat"] - trip_diff["stat"])
  ddf   <- as.numeric(trip_sys["df"]   - trip_diff["df"])
  pval  <- if(is.finite(dstat) && is.finite(ddf) && ddf > 0) pchisq(dstat, df=ddf, lower.tail=FALSE) else NA_real_
  c(DiffH_stat=dstat, DiffH_df=ddf, DiffH_p=pval)
}

run_panel <- function(path, panel){
  message(">>> Reading panel: ", panel, " (", path, ")")
  df <- read_panel(path) %>% standardize_vars()
  
  dvs_present   <- intersect(ALL_DVS,  names(df))
  ctrls_present <- intersect(ALL_CTRL, names(df))
  
  if(length(dvs_present)==0){
    message("No target DVs present in ", panel, " — skipping.")
    return(tibble::tibble(
      panel=panel, dv=ALL_DVS, N=NA_integer_, countries=length(unique(df$country)),
      n_instruments=NA_integer_, AR1_p=NA_real_, AR2_p=NA_real_, Hansen_p=NA_real_,
      Hansen_stat=NA_real_, Hansen_df=NA_real_,
      DiffH_stat=NA_real_, DiffH_df=NA_real_, DiffH_p=NA_real_,
      passes=FALSE, status="dv_missing", transformation=NA_character_, lags_used=NA_character_,
      controls_used=NA_character_, time_fe=NA_character_, time_strategy=NA_character_
    ))
  }
  if(length(ctrls_present)==0){
    message("Note (", panel, "): no listed controls found; models = lag(DV,1) + time.")
  } else {
    message("Controls used (", panel, "): ", paste(ctrls_present, collapse=", "))
  }
  
  diag_rows <- list()
  
  # mark any missing DVs explicitly
  for(dv in setdiff(ALL_DVS, dvs_present)){
    diag_rows[[length(diag_rows)+1]] <- data.frame(
      panel=panel, dv=dv, N=NA_integer_, countries=length(unique(df$country)),
      n_instruments=NA_integer_, AR1_p=NA_real_, AR2_p=NA_real_, Hansen_p=NA_real_,
      Hansen_stat=NA_real_, Hansen_df=NA_real_,
      DiffH_stat=NA_real_, DiffH_df=NA_real_, DiffH_p=NA_real_,
      passes=FALSE, status="dv_missing", transformation=NA_character_,
      lags_used=NA_character_, controls_used=NA_character_, time_fe=NA_character_, time_strategy=NA_character_,
      stringsAsFactors=FALSE
    )
  }
  
  for(dv in dvs_present){
    success <- FALSE
    attempt_log <- character(0)
    chosen_res <- NULL
    
    # try time-IV strategies in order
    for(time_strategy in TIME_STRATS){
      # try baseline/robust lag windows
      for(spec in list(
        list(tf="ld", L=2, U=2),
        list(tf="ld", L=2, U=3),
        list(tf="ld", L=3, U=4),
        list(tf="d",  L=2, U=3)
      )){
        a <- attempt_pgmm(df, panel, dv, L=spec$L, U=spec$U, tf=spec$tf,
                          ctrls_available=ctrls_present, time_strategy=time_strategy)
        attempt_log <- c(attempt_log, a$status)
        if(isTRUE(a$ok)){
          chosen_res <- a
          write_outputs_if_pass(a, panel, dv)
          success <- TRUE
          break
        }
      }
      if(success) break
    }
    
    if(success){
      # ===== NEW: Difference-in-Hansen (System vs Difference), same lags & same time strategy =====
      dih <- c(DiffH_stat=NA_real_, DiffH_df=NA_real_, DiffH_p=NA_real_)
      if(identical(chosen_res$tf, "ld")){  # only meaningful for System GMM
        L <- as.integer(strsplit(chosen_res$diag$lags_used,":")[[1]][1])
        U <- as.integer(strsplit(chosen_res$diag$lags_used,":")[[1]][2])
        res_diff <- attempt_pgmm(df, panel, dv, L=L, U=U, tf="d",
                                 ctrls_available=ctrls_present,
                                 time_strategy=chosen_res$time_strategy)
        if(!inherits(res_diff$fit, "error") && !is.null(res_diff$fit)) {
          # use your helper to compute DIH:
          dih <- compute_diff_hansen(chosen_res, res_diff)
        }
      }
      
      # add diagnostics row (with DIH fields filled)
      drow <- chosen_res$diag
      drow$DiffH_stat <- dih[["DiffH_stat"]]
      drow$DiffH_df   <- dih[["DiffH_df"]]
      drow$DiffH_p    <- dih[["DiffH_p"]]
      diag_rows[[length(diag_rows)+1]] <- drow
      
      # coefficient plot (unchanged)
      coefs <- chosen_res$coef_tab
      if(nrow(coefs)){
        p <- ggplot(coefs, aes(x=term, y=beta, ymin=beta-se, ymax=beta+se)) +
          geom_pointrange() + geom_hline(yintercept=0, linetype="dashed") +
          theme_minimal() + labs(title=paste(panel, dv, "— Coefficients"), x=NULL, y="β (±1 SE)") +
          theme(axis.text.x = element_text(angle=45, hjust=1))
        ggsave(filename=file.path("out_figs", sprintf("%s_coeffs_%s.png", panel, dv)), plot=p,
               width=7, height=5, dpi=300)
      }
    } else {
      # everything failed for this DV
      diag_rows[[length(diag_rows)+1]] <- data.frame(
        panel=panel, dv=dv, N=NA_integer_, countries=length(unique(df$country)),
        n_instruments=NA_integer_, AR1_p=NA_real_, AR2_p=NA_real_, Hansen_p=NA_real_,
        Hansen_stat=NA_real_, Hansen_df=NA_real_,
        DiffH_stat=NA_real_, DiffH_df=NA_real_, DiffH_p=NA_real_,
        passes=FALSE, status=paste0("all_failed: ", paste(attempt_log, collapse=" | ")),
        transformation=NA_character_, lags_used=NA_character_,
        controls_used=paste(ctrls_present, collapse="+"),
        time_fe="all_strategies_failed", time_strategy=NA_character_, stringsAsFactors=FALSE
      )
    }
  }
  
  diag_df <- dplyr::bind_rows(diag_rows)
  plot_and_save(diag_df, panel)
  diag_df
}

# ===== Run all panels that exist =====
existing <- INPUTS[file.exists(INPUTS)]
if(length(existing)==0) stop_clean("None of the expected input CSVs are in the working directory.")
all_diags <- purrr::imap_dfr(existing, run_panel)

# Combined diagnostics summary
readr::write_csv(all_diags, file.path("out_models","gmm_diagnostics_summary_all.csv"))
message("\n=== Combined GMM Diagnostics Summary ===")
print(all_diags %>% dplyr::select(panel, dv, N, countries, n_instruments,
                                  AR1_p, AR2_p, Hansen_p, DiffH_p,
                                  transformation, lags_used, controls_used, time_fe, status))

passed <- all_diags %>% dplyr::filter(passes)
if(nrow(passed)>0){
  message("\nPassed models:")
  print(passed %>% dplyr::select(panel, dv, AR1_p, AR2_p, Hansen_p, DiffH_p, n_instruments,
                                 transformation, lags_used, controls_used, time_fe))
} else {
  message("\nNo models passed guardrails.")
}

message("\nDone.")

