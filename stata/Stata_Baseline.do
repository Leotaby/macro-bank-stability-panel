* =============================================================
* MACRO DETERMINANTS OF BANK STABILITY — FINAL PANEL SCRIPT
* Tables 8–12: FE + Interaction + System-GMM with LaTeX export
* Author: Hatef (Leo) Tabbakhian | Supervisor: Prof. Simonelli
* =============================================================

clear all
set more off
cd "/Users/nolantaby/Documents/01-Digital Marketing/Thesis/Last Datas/"

* -------------------------------------------------------------
* STEP 1: Import data with country-specific real rate
* -------------------------------------------------------------
import delimited "Euro_B_stability_master_with_real_rate.csv", clear case(lower)

* Set panel structure
encode country, gen(cntry_id)
xtset cntry_id year

* Basic vars
gen log_bank_z_score = ln(bank_z_score) if bank_z_score > 0 & bank_z_score < .
gen L1_credit_growth = L.credit_growth

label var gdp_growth "GDP growth (pp)"
label var unemployment "Unemployment (pp)"
label var inflation "Inflation (pp)"
label var real_rate "Real short rate (pp)"
label var L1_credit_growth "L1 credit growth (pp)"

* Interaction for Table 9
egen med_unemp = median(unemployment)
gen high_slack = unemployment > med_unemp
gen rate_x_slack = real_rate * high_slack
label var rate_x_slack "Real rate × High-slack"
drop med_unemp

* =============================================================
* STEP 2: Table 8 — Baseline Two-Way FE
* =============================================================
eststo clear

eststo fe_z: reghdfe log_bank_z_score gdp_growth unemployment inflation ///
    real_rate L1_credit_growth, absorb(cntry_id year) vce(cluster cntry_id)

eststo fe_npl: reghdfe npl_ratio_filled gdp_growth unemployment inflation ///
    real_rate L1_credit_growth, absorb(cntry_id year) vce(cluster cntry_id)

eststo fe_tier1: reghdfe capital_adequacy_ratio_filled gdp_growth unemployment ///
    inflation real_rate L1_credit_growth, absorb(cntry_id year) vce(cluster cntry_id)

esttab fe_z fe_npl fe_tier1 using "Table8_FE.tex", ///
    replace label b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    nomtitles nonumbers drop(_cons) ///
    booktabs alignment(D{.}{.}{-1}) compress

* =============================================================
* STEP 3: Table 9 — FE + Real Rate × High-Slack
* =============================================================
eststo clear

eststo int_z: reghdfe log_bank_z_score gdp_growth unemployment inflation ///
    real_rate L1_credit_growth rate_x_slack, absorb(cntry_id year) vce(cluster cntry_id)

eststo int_npl: reghdfe npl_ratio_filled gdp_growth unemployment inflation ///
    real_rate L1_credit_growth rate_x_slack, absorb(cntry_id year) vce(cluster cntry_id)

eststo int_tier1: reghdfe capital_adequacy_ratio_filled gdp_growth unemployment ///
    inflation real_rate L1_credit_growth rate_x_slack, absorb(cntry_id year) vce(cluster cntry_id)

esttab int_z int_npl int_tier1 using "Table9_FE_interaction.tex", ///
    replace label b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    nomtitles nonumbers drop(_cons) ///
    booktabs alignment(D{.}{.}{-1}) compress

* =============================================================
* STEP 4: System-GMM — Tables 10, 11, 12
* =============================================================

* Reload data fresh for GMM stage
import delimited "Euro_B_stability_master_with_real_rate.csv", clear case(lower)
encode country, gen(cntry_id)
xtset cntry_id year

* Recreate variables needed for GMM
gen log_bank_z_score = ln(bank_z_score) if bank_z_score > 0 & bank_z_score < .
label var log_bank_z_score "Log of bank Z-score"

gen L1_credit_growth = L.credit_growth
label var L1_credit_growth "L1 credit growth (pp)"

gen D_credit_growth = D.credit_growth
gen D_credit_to_gdp_gap = D.credit_to_gdp_gap
gen D_real_rate = D.real_rate

* =============================================================
* TABLE 10: System-GMM — Z-score
* =============================================================
eststo clear

xtabond2 log_bank_z_score L.log_bank_z_score ///
    D.(gdp_growth unemployment inflation real_rate credit_growth credit_to_gdp_gap), ///
    gmm(L.log_bank_z_score D.credit_growth D.credit_to_gdp_gap, collapse lag(2 .)) ///
    ivstyle(D.gdp_growth D.unemployment D.inflation D.real_rate, equation(level)) ///
    twostep robust small

* Export GMM Z-score
esttab using "Table10_GMM_zscore.tex", replace ///
    label b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    nomtitles nonumbers drop(_cons) ///
    booktabs alignment(D{.}{.}{-1}) compress

* =============================================================
* TABLE 11: System-GMM — NPL ratio
* =============================================================
xtabond2 npl_ratio_filled L.npl_ratio_filled ///
    D.(gdp_growth unemployment inflation real_rate credit_growth credit_to_gdp_gap), ///
    gmm(L.npl_ratio_filled D.credit_growth D.credit_to_gdp_gap, collapse lag(2 .)) ///
    ivstyle(D.gdp_growth D.unemployment D.inflation D.real_rate, equation(level)) ///
    twostep robust small

esttab using "Table11_GMM_npl.tex", replace ///
    label b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    nomtitles nonumbers drop(_cons) ///
    booktabs alignment(D{.}{.}{-1}) compress

* =============================================================
* TABLE 12: System-GMM — Tier 1 capital ratio
* =============================================================
xtabond2 capital_adequacy_ratio_filled L.capital_adequacy_ratio_filled ///
    D.(gdp_growth unemployment inflation real_rate credit_growth credit_to_gdp_gap), ///
    gmm(L.capital_adequacy_ratio_filled D.credit_growth D.credit_to_gdp_gap, collapse lag(2 .)) ///
    ivstyle(D.gdp_growth D.unemployment D.inflation D.real_rate, equation(level)) ///
    twostep robust small

esttab using "Table12_GMM_tier1.tex", replace ///
    label b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    nomtitles nonumbers drop(_cons) ///
    booktabs alignment(D{.}{.}{-1}) compress
