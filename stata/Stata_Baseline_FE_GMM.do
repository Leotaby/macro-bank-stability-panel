* =============================================================
* MACRO DETERMINANTS OF BANK STABILITY (EA-20, 2000–2022)
* Author: Hatef (Leo) Tabbakhian | Supervisor: Prof. Simonelli
* Reproduces Tables 8–12: Baseline FE, FE+Interaction, System-GMM
* =============================================================

clear all
set more off
cd "/Users/nolantaby/Documents/01-Digital Marketing/Thesis/Last Datas/"

import delimited "Euro_B_stability_master_with_real_rate.csv", clear case(lower)

encode country, gen(cntry_id)
xtset cntry_id year

* Create variables

* log(Z-score)
gen log_bank_z_score = ln(bank_z_score) if bank_z_score > 0 & bank_z_score < .
label var log_bank_z_score "Log of bank Z-score"

* Lag credit growth
gen L1_credit_growth = L.credit_growth

* Median-based high labor slack dummy
egen med_unemp = median(unemployment)
gen high_slack = unemployment > med_unemp
drop med_unemp

* Interaction term with real rate (note: omitted in FE due to collinearity)
gen rate_x_slack = real_rate * high_slack

* =============================================================
* TABLE 8: Baseline Two-Way FE (Note: real_rate omitted)
* =============================================================
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


esttab FE_Z FE_NPL FE_T1 using "Table8_FE.tex", replace ///
    label b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    nomtitles nonumbers drop(_cons) booktabs alignment(D{.}{.}{-1}) compress ///
    stats(N r2_within, labels("Obs." "Within R^2")) ///
    addnotes("Country and year fixed effects included.", ///
             "SEs clustered by country.", ///
             "The real short rate is collinear with year FE (= ECB policy minus HICP); omitted by construction.")

* =============================================================
* TABLE 9: FE + Interaction
* =============================================================
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

esttab INT_Z INT_NPL INT_T1 using "Table9_FE_interaction.tex", replace ///
    label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    nomtitles nonumbers drop(_cons) booktabs alignment(D{.}{.}{-1}) compress ///
    addnotes("Country and year fixed effects included; SEs clustered by country.", ///
             "Real short rate omitted in FE due to collinearity with year FE.")

* =============================================================
* TABLES 10–12: System-GMM (Disciplined Instruments)
* =============================================================

* Create 4-year bins for time FE
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
estadd scalar AR2    = e(m2p)
estadd scalar Hansen = e(Hansenp)
estadd scalar Instr  = e(j)
eststo GMM_Z

* ---------- Table 11: GMM for NPL ----------
xtabond2 npl_ratio_filled L.npl_ratio_filled ///
    gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap ///
    i.bin4, ///
    gmm(L.npl_ratio_filled, lag(2 3) collapse) ///
    ivstyle(gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap, eq(level)) ///
    ivstyle(i.bin4, eq(level)) ///
    twostep robust small
estadd scalar AR2    = e(m2p)
estadd scalar Hansen = e(Hansenp)
estadd scalar Instr  = e(j)
eststo GMM_NPL

* ---------- Table 12: GMM for Tier-1 ----------
xtabond2 capital_adequacy_ratio_filled L.capital_adequacy_ratio_filled ///
    gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap ///
    i.bin4, ///
    gmm(L.capital_adequacy_ratio_filled, lag(2 3) collapse) ///
    ivstyle(gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap, eq(level)) ///
    ivstyle(i.bin4, eq(level)) ///
    twostep robust small
estadd scalar AR2    = e(m2p)
estadd scalar Hansen = e(Hansenp)
estadd scalar Instr  = e(j)
eststo GMM_T1

* ---------- LaTeX exports ----------
esttab GMM_Z using "Table10_GMM_zscore.tex", replace ///
    label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    nomtitles nonumbers drop(_cons) booktabs alignment(D{.}{.}{-1}) compress ///
    stats(N Instr AR2 Hansen, labels("Obs." "# instr." "AR(2) p" "Hansen p")) ///
    addnotes("Two-step System-GMM with Windmeijer correction.", ///
             "GMM: L.y with lags 2–3, collapsed; controls as standard IVs.", ///
             "Time FE: 4-year bins as IVs; instrument count constrained (< N).")

esttab GMM_NPL using "Table11_GMM_npl.tex", replace ///
    label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    nomtitles nonumbers drop(_cons) booktabs alignment(D{.}{.}{-1}) compress ///
    stats(N Instr AR2 Hansen, labels("Obs." "# instr." "AR(2) p" "Hansen p")) ///
    addnotes("Two-step System-GMM with Windmeijer correction.", ///
             "GMM: L.y with lags 2–3, collapsed; controls as standard IVs.", ///
             "Time FE: 4-year bins as IVs; instrument count constrained (< N).")

esttab GMM_T1 using "Table12_GMM_tier1.tex", replace ///
    label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    nomtitles nonumbers drop(_cons) booktabs alignment(D{.}{.}{-1}) compress ///
    stats(N Instr AR2 Hansen, labels("Obs." "# instr." "AR(2) p" "Hansen p")) ///
    addnotes("Two-step System-GMM with Windmeijer correction.", ///
             "GMM: L.y with lags 2–3, collapsed; controls as standard IVs.", ///
             "Time FE: 4-year bins as IVs; instrument count constrained (< N).")

* END OF SCRIPT
