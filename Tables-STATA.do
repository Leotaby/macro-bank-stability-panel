*******************************************************
* MACRO DETERMINANTS OF BANK STABILITY (EA-20, 2000–2022)
* Author: Hatef (Leo) Tabbakhian | Supervisor: Prof. Simonelli
* Reproduces Tables 8–12: FE baseline, FE+interaction, System-GMM
* Output: Table8_FE.tex, Table9_FE_interaction.tex,
*         Table10_GMM_zscore.tex, Table11_GMM_npl.tex, Table12_GMM_tier1.tex
*
* LaTeX preamble: \usepackage{booktabs}\usepackage{siunitx}
* (or use dcolumn and swap alignment to D{.}{.}{-1})
*******************************************************

version 17.0
clear all
set more off

* -------- Paths --------
cd "/Users/nolantaby/Documents/01-Digital Marketing/Thesis/Last Datas/"

* -------- Ensure required packages --------
cap which esttab
if _rc ssc install estout, replace

cap which reghdfe
if _rc ssc install reghdfe, replace

cap which xtabond2
if _rc ssc install xtabond2, replace

* -------- Load data --------
import delimited "Euro_B_stability_master_with_real_rate.csv", clear case(lower)

* Panel settings
encode country, gen(cntry_id)
xtset cntry_id year

* -------- Variable construction --------

* log(Z-score) safely (only positive values)
gen log_bank_z_score = ln(bank_z_score) if bank_z_score > 0 & bank_z_score < .
label var log_bank_z_score "Log Z-score"

* Lagged credit growth
gen L1_credit_growth = L.credit_growth
label var L1_credit_growth "Credit growth (t−1, %)"

* Median-based high labor slack dummy
egen med_unemp = median(unemployment)
gen high_slack  = unemployment > med_unemp
drop med_unemp
label var high_slack "High labor slack (above median)"

* Interaction: real rate × high slack (collinear with year FE in FE models)
gen rate_x_slack = real_rate * high_slack
label var rate_x_slack "Real rate × high slack"

* Nice labels for regressors (used in tables)
label var gdp_growth            "GDP growth (%)"
label var unemployment          "Unemployment (pp)"
label var inflation             "Inflation (pp)"
label var real_rate             "Real short rate (pp)"
label var credit_to_gdp_gap     "Credit-to-GDP gap (pp)"
label var npl_ratio_filled      "NPL ratio (%)"
label var capital_adequacy_ratio_filled "Tier-1 ratio (%)"

* ============================================================
* TABLE 8 — Baseline Two-Way FE (real_rate omitted by design)
* ============================================================
eststo clear

reghdfe log_bank_z_score gdp_growth unemployment inflation L1_credit_growth, ///
    absorb(cntry_id year) vce(cluster cntry_id)
eststo FE_Z

reghdfe npl_ratio_filled gdp_growth unemployment inflation L1_credit_growth, ///
    absorb(cntry_id year) vce(cluster cntry_id)
eststo FE_NPL

reghdfe capital_adequacy_ratio_filled gdp_growth unemployment inflation L1_credit_growth, ///
    absorb(cntry_id year) vce(cluster cntry_id)
eststo FE_T1

* Export Table 8 (use siunitx alignment "S")
esttab FE_Z FE_NPL FE_T1 using "out_tex/Table8_FE.tex", replace ///
    label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("log Z" "NPL (%)" "Tier-1 (%)") ///
    booktabs alignment(S) compress ///
    stats(N r2_within, labels("Obs." "Within $R^2$") fmt(0 3)) ///
    addnotes("Standard errors in parentheses.", ///
             "Country and year fixed effects included; SEs clustered by country.", ///
             "Real short rate is collinear with year FE (= ECB policy minus HICP); omitted by construction.")

* ============================================================
* TABLE 9 — FE with interaction (state dependence via slack)
* ============================================================
eststo clear

reghdfe log_bank_z_score gdp_growth unemployment inflation L1_credit_growth rate_x_slack, ///
    absorb(cntry_id year) vce(cluster cntry_id)
eststo INT_Z

reghdfe npl_ratio_filled gdp_growth unemployment inflation L1_credit_growth rate_x_slack, ///
    absorb(cntry_id year) vce(cluster cntry_id)
eststo INT_NPL

reghdfe capital_adequacy_ratio_filled gdp_growth unemployment inflation L1_credit_growth rate_x_slack, ///
    absorb(cntry_id year) vce(cluster cntry_id)
eststo INT_T1

esttab INT_Z INT_NPL INT_T1 using "out_tex/Table9_FE_interaction.tex", replace ///
    label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("log Z (FE+int.)" "NPL (FE+int.)" "Tier-1 (FE+int.)") ///
    booktabs alignment(S) compress ///
    addnotes("Country and year fixed effects included; SEs clustered by country.", ///
             "Real short rate omitted in FE due to collinearity with year FE; interaction term shown as robustness check.")

* ============================================================
* TABLES 10–12 — System-GMM (disciplined instruments)
*  - Two-step, Windmeijer-robust (robust small)
*  - Collapsed GMM, lag(2 3), time FE as 4-yr bins (IV in levels)
*  - #instruments constrained (< #countries)
*  - Correctly prints AR(2) and Hansen via e(m2p) and e(hansenp)
* ============================================================

* Time FE as 4-year bins
gen bin4 = floor((year-2000)/4) if inrange(year,2000,2022)
label define bin4 0 "2000–2003" 1 "2004–2007" 2 "2008–2011" 3 "2012–2015" 4 "2016–2019" 5 "2020–2022"
label values bin4 bin4

eststo clear

* ---------- Table 10: GMM for log Z ----------
xtabond2 log_bank_z_score L.log_bank_z_score ///
    gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap ///
    i.bin4, ///
    gmm(L.log_bank_z_score, lag(2 3) collapse) ///
    ivstyle(gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap, eq(level)) ///
    ivstyle(i.bin4, eq(level)) ///
    twostep robust small
eststo GMM_Z

* ---------- Table 11: GMM for NPL ----------
xtabond2 npl_ratio_filled L.npl_ratio_filled ///
    gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap ///
    i.bin4, ///
    gmm(L.npl_ratio_filled, lag(2 3) collapse) ///
    ivstyle(gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap, eq(level)) ///
    ivstyle(i.bin4, eq(level)) ///
    twostep robust small
eststo GMM_NPL

* ---------- Table 12: GMM for Tier-1 ----------
xtabond2 capital_adequacy_ratio_filled L.capital_adequacy_ratio_filled ///
    gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap ///
    i.bin4, ///
    gmm(L.capital_adequacy_ratio_filled, lag(2 3) collapse) ///
    ivstyle(gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap, eq(level)) ///
    ivstyle(i.bin4, eq(level)) ///
    twostep robust small
eststo GMM_T1

* ---------- LaTeX exports (print N, #instr, AR(2) p, Hansen p) ----------
* Note: xtabond2 stores N=e(N), j=e(j), m2p=e(m2p), hansenp=e(hansenp)

esttab GMM_Z using "out_tex/Table10_GMM_zscore.tex", replace ///
    label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("log Z (Sys-GMM)") ///
    booktabs alignment(S) compress ///
    stats(N j m2p hansenp, labels("Obs." "# instr." "AR(2) p" "Hansen p") fmt(0 0 3 3)) ///
    addnotes("Two-step System-GMM with Windmeijer correction.", ///
             "GMM: L.y lag(2–3), collapsed; controls as standard IVs.", ///
             "Time FE: 4-year bins as IVs; instrument count constrained (< number of countries).")

esttab GMM_NPL using "out_tex/Table11_GMM_npl.tex", replace ///
    label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("NPL (Sys-GMM)") ///
    booktabs alignment(S) compress ///
    stats(N j m2p hansenp, labels("Obs." "# instr." "AR(2) p" "Hansen p") fmt(0 0 3 3)) ///
    addnotes("Two-step System-GMM with Windmeijer correction.", ///
             "GMM: L.y lag(2–3), collapsed; controls as standard IVs.", ///
             "Time FE: 4-year bins as IVs; instrument count constrained (< number of countries).")

esttab GMM_T1 using "out_tex/Table12_GMM_tier1.tex", replace ///
    label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("Tier-1 (Sys-GMM)") ///
    booktabs alignment(S) compress ///
    stats(N j m2p hansenp, labels("Obs." "# instr." "AR(2) p" "Hansen p") fmt(0 0 3 3)) ///
    addnotes("Two-step System-GMM with Windmeijer correction.", ///
             "GMM: L.y lag(2–3), collapsed; controls as standard IVs.", ///
             "Time FE: 4-year bins as IVs; instrument count constrained (< number of countries).")

* ============================================================
* OPTIONAL ROBUSTNESS (keep on hand if asked in viva)
* Tighter lag window L(2 2) for NPL to further discipline instruments
* (often moves Hansen p upward while keeping signs)
* ============================================================
capture noisily {
    xtabond2 npl_ratio_filled L.npl_ratio_filled ///
        gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap ///
        i.bin4, ///
        gmm(L.npl_ratio_filled, lag(2 2) collapse) ///
        ivstyle(gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap, eq(level)) ///
        ivstyle(i.bin4, eq(level)) ///
        twostep robust small
    eststo GMM_NPL_L22

    esttab GMM_NPL GMM_NPL_L22 using "Table11_GMM_npl_alt.tex", replace ///
        label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
        mlabels("NPL (GMM L2–3)" "NPL (GMM L2–2)") ///
        nomtitles nonumbers drop(_cons) booktabs alignment(S) compress ///
        stats(N j m2p hansenp, labels("Obs." "# instr." "AR(2) p" "Hansen p") fmt(0 0 3 3)) ///
        addnotes("Two-step System-GMM (Windmeijer). GMM(L.y) instruments collapsed; lag windows shown in headers.", ///
                 "Time FE: 4-year bins as IVs; instrument counts kept below country count.")
}

display as text "----- Done. LaTeX tables written to current directory. -----"
