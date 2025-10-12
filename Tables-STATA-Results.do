
  ___  ____  ____  ____  ____ ®
 /__    /   ____/   /   ____/      StataNow 19.5
___/   /   /___/   /   /___/       SE—Standard Edition

 Statistics and Data Science       Copyright 1985-2025 StataC
> orp LLC
                                   StataCorp
                                   4905 Lakeway Drive
                                   College Station, Texas 778
> 45 USA
                                   800-782-8272        https:
> //www.stata.com
                                   979-696-4600        servic
> e@stata.com

Stata license: Single-user network, expiring 13 Oct 2025
Serial number: 401909342203
  Licensed to: Leo Taby
               University of Naples Federico II

Notes:
      1. Unicode is supported; see help unicode_advice.
      2. Maximum number of variables is set to 5,000 but
          can be increased; see help set_maxvar.
      3. New update available; type -update all-

. do "/Users/nolantaby/Documents/01-Digital Marketing/Thesis/Last Datas/STATA_Baseline_FE_GMM_AR_HSN2.do"

. *******************************************************
. * MACRO DETERMINANTS OF BANK STABILITY (EA-20, 2000–2022)
. * Author: Hatef (Leo) Tabbakhian | Supervisor: Prof. Simonelli
. * Reproduces Tables 8–12: FE baseline, FE+interaction, System-GMM
. * Output: Table8_FE.tex, Table9_FE_interaction.tex,
. *         Table10_GMM_zscore.tex, Table11_GMM_npl.tex, Table12_GMM_tier1.tex
. *
. * LaTeX preamble: \usepackage{booktabs}\usepackage{siunitx}
. * (or use dcolumn and swap alignment to D{.}{.}{-1})
. *******************************************************
. 
. version 17.0

. clear all

. set more off

. 
. * -------- Paths --------
. cd "/Users/nolantaby/Documents/01-Digital Marketing/Thesis/Last Datas/"
/Users/nolantaby/Documents/01-Digital Marketing/Thesis/Last Datas

. 
. * -------- Ensure required packages --------
. cap which esttab

. if _rc ssc install estout, replace

. 
. cap which reghdfe

. if _rc ssc install reghdfe, replace

. 
. cap which xtabond2

. if _rc ssc install xtabond2, replace

. 
. * -------- Load data --------
. import delimited "Euro_B_stability_master_with_real_rate.csv", clear case(lower)
(encoding automatically selected: ISO-8859-1)
(22 vars, 460 obs)

. 
. * Panel settings
. encode country, gen(cntry_id)

. xtset cntry_id year

Panel variable: cntry_id (strongly balanced)
 Time variable: year, 2000 to 2022
         Delta: 1 unit

. 
. * -------- Variable construction --------
. 
. * log(Z-score) safely (only positive values)
. gen log_bank_z_score = ln(bank_z_score) if bank_z_score > 0 & bank_z_score < .
(27 missing values generated)

. label var log_bank_z_score "Log Z-score"

. 
. * Lagged credit growth
. gen L1_credit_growth = L.credit_growth
(93 missing values generated)

. label var L1_credit_growth "Credit growth (t−1, %)"

. 
. * Median-based high labor slack dummy
. egen med_unemp = median(unemployment)

. gen high_slack  = unemployment > med_unemp

. drop med_unemp

. label var high_slack "High labor slack (above median)"

. 
. * Interaction: real rate × high slack (collinear with year FE in FE models)
. gen rate_x_slack = real_rate * high_slack

. label var rate_x_slack "Real rate × high slack"

. 
. * Nice labels for regressors (used in tables)
. label var gdp_growth            "GDP growth (%)"

. label var unemployment          "Unemployment (pp)"

. label var inflation             "Inflation (pp)"

. label var real_rate             "Real short rate (pp)"

. label var credit_to_gdp_gap     "Credit-to-GDP gap (pp)"

. label var npl_ratio_filled      "NPL ratio (%)"

. label var capital_adequacy_ratio_filled "Tier-1 ratio (%)"

. 
. * ============================================================
. * TABLE 8 — Baseline Two-Way FE (real_rate omitted by design)
. * ============================================================
. eststo clear

. 
. reghdfe log_bank_z_score gdp_growth unemployment inflation L1_credit_growth, ///
>     absorb(cntry_id year) vce(cluster cntry_id)
(MWFE estimator converged in 6 iterations)

HDFE Linear regression                            Number of obs   =        342
Absorbing 2 HDFE groups                           F(   4,     19) =       3.14
Statistics robust to heteroskedasticity           Prob > F        =     0.0384
                                                  R-squared       =     0.7135
                                                  Adj R-squared   =     0.6732
                                                  Within R-sq.    =     0.0837
Number of clusters (cntry_id) =         20        Root MSE        =     0.4169

                                  (Std. err. adjusted for 20 clusters in cntry_id)
----------------------------------------------------------------------------------
                 |               Robust
log_bank_z_score | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
-----------------+----------------------------------------------------------------
      gdp_growth |   .0308051   .0102425     3.01   0.007     .0093674    .0522428
    unemployment |  -.0077627    .014296    -0.54   0.593    -.0376846    .0221592
       inflation |   .0460475   .0338793     1.36   0.190    -.0248628    .1169577
L1_credit_growth |  -.0060526   .0061349    -0.99   0.336     -.018893    .0067878
           _cons |   2.421684   .0941663    25.72   0.000     2.224591    2.618776
----------------------------------------------------------------------------------

Absorbed degrees of freedom:
-----------------------------------------------------+
 Absorbed FE | Categories  - Redundant  = Num. Coefs |
-------------+---------------------------------------|
    cntry_id |        20          20           0    *|
        year |        20           1          19     |
-----------------------------------------------------+
* = FE nested within cluster; treated as redundant for DoF computation

. eststo FE_Z

. 
. reghdfe npl_ratio_filled gdp_growth unemployment inflation L1_credit_growth, ///
>     absorb(cntry_id year) vce(cluster cntry_id)
(MWFE estimator converged in 5 iterations)

HDFE Linear regression                            Number of obs   =        367
Absorbing 2 HDFE groups                           F(   4,     19) =       2.80
Statistics robust to heteroskedasticity           Prob > F        =     0.0555
                                                  R-squared       =     0.6988
                                                  Adj R-squared   =     0.6587
                                                  Within R-sq.    =     0.3379
Number of clusters (cntry_id) =         20        Root MSE        =     4.4116

                                  (Std. err. adjusted for 20 clusters in cntry_id)
----------------------------------------------------------------------------------
                 |               Robust
npl_ratio_filled | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
-----------------+----------------------------------------------------------------
      gdp_growth |   -.011348   .0702568    -0.16   0.873    -.1583972    .1357013
    unemployment |   1.251944   .4309488     2.91   0.009      .349958     2.15393
       inflation |   .2035969   .1780307     1.14   0.267    -.1690257    .5762195
L1_credit_growth |  -.0377457   .0580583    -0.65   0.523    -.1592632    .0837718
           _cons |  -5.409271    3.73257    -1.45   0.164    -13.22163    2.403088
----------------------------------------------------------------------------------

Absorbed degrees of freedom:
-----------------------------------------------------+
 Absorbed FE | Categories  - Redundant  = Num. Coefs |
-------------+---------------------------------------|
    cntry_id |        20          20           0    *|
        year |        21           1          20     |
-----------------------------------------------------+
* = FE nested within cluster; treated as redundant for DoF computation

. eststo FE_NPL

. 
. reghdfe capital_adequacy_ratio_filled gdp_growth unemployment inflation L1_credit_growth, ///
>     absorb(cntry_id year) vce(cluster cntry_id)
(MWFE estimator converged in 5 iterations)

HDFE Linear regression                            Number of obs   =        367
Absorbing 2 HDFE groups                           F(   4,     19) =       6.86
Statistics robust to heteroskedasticity           Prob > F        =     0.0014
                                                  R-squared       =     0.8708
                                                  Adj R-squared   =     0.8536
                                                  Within R-sq.    =     0.1010
Number of clusters (cntry_id) =         20        Root MSE        =     1.7604

                                  (Std. err. adjusted for 20 clusters in cntry_id)
----------------------------------------------------------------------------------
                 |               Robust
capital_adequa~d | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
-----------------+----------------------------------------------------------------
      gdp_growth |   .0611562   .0563787     1.08   0.292    -.0568457    .1791581
    unemployment |  -.1636349   .0604748    -2.71   0.014    -.2902102   -.0370597
       inflation |  -.1654216   .1458874    -1.13   0.271    -.4707675    .1399242
L1_credit_growth |  -.0635264   .0367035    -1.73   0.100    -.1403478     .013295
           _cons |   18.38097   .8433835    21.79   0.000     16.61574    20.14619
----------------------------------------------------------------------------------

Absorbed degrees of freedom:
-----------------------------------------------------+
 Absorbed FE | Categories  - Redundant  = Num. Coefs |
-------------+---------------------------------------|
    cntry_id |        20          20           0    *|
        year |        21           1          20     |
-----------------------------------------------------+
* = FE nested within cluster; treated as redundant for DoF computation

. eststo FE_T1

. 
. * Export Table 8 (use siunitx alignment "S")
. esttab FE_Z FE_NPL FE_T1 using "out_tex/Table8_FE.tex", replace ///
>     label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
>     mtitles("log Z" "NPL (%)" "Tier-1 (%)") ///
>     booktabs alignment(S) compress ///
>     stats(N r2_within, labels("Obs." "Within $R^2$") fmt(0 3)) ///
>     addnotes("Standard errors in parentheses.", ///
>              "Country and year fixed effects included; SEs clustered by country.", ///
>              "Real short rate is collinear with year FE (= ECB policy minus HICP); omitted by construction.")
(output written to out_tex/Table8_FE.tex)

. 
. * ============================================================
. * TABLE 9 — FE with interaction (state dependence via slack)
. * ============================================================
. eststo clear

. 
. reghdfe log_bank_z_score gdp_growth unemployment inflation L1_credit_growth rate_x_slack, ///
>     absorb(cntry_id year) vce(cluster cntry_id)
(MWFE estimator converged in 6 iterations)

HDFE Linear regression                            Number of obs   =        342
Absorbing 2 HDFE groups                           F(   5,     19) =       3.48
Statistics robust to heteroskedasticity           Prob > F        =     0.0213
                                                  R-squared       =     0.7176
                                                  Adj R-squared   =     0.6768
                                                  Within R-sq.    =     0.0968
Number of clusters (cntry_id) =         20        Root MSE        =     0.4146

                                  (Std. err. adjusted for 20 clusters in cntry_id)
----------------------------------------------------------------------------------
                 |               Robust
log_bank_z_score | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
-----------------+----------------------------------------------------------------
      gdp_growth |   .0250461   .0097439     2.57   0.019     .0046518    .0454404
    unemployment |  -.0068956   .0149281    -0.46   0.649    -.0381405    .0243494
       inflation |   .0220213   .0168901     1.30   0.208    -.0133301    .0573726
L1_credit_growth |  -.0047162   .0050596    -0.93   0.363    -.0153061    .0058737
    rate_x_slack |  -.0630256   .0586392    -1.07   0.296    -.1857588    .0597075
           _cons |   2.457816   .1236326    19.88   0.000      2.19905    2.716582
----------------------------------------------------------------------------------

Absorbed degrees of freedom:
-----------------------------------------------------+
 Absorbed FE | Categories  - Redundant  = Num. Coefs |
-------------+---------------------------------------|
    cntry_id |        20          20           0    *|
        year |        20           1          19     |
-----------------------------------------------------+
* = FE nested within cluster; treated as redundant for DoF computation

. eststo INT_Z

. 
. reghdfe npl_ratio_filled gdp_growth unemployment inflation L1_credit_growth rate_x_slack, ///
>     absorb(cntry_id year) vce(cluster cntry_id)
(MWFE estimator converged in 5 iterations)

HDFE Linear regression                            Number of obs   =        367
Absorbing 2 HDFE groups                           F(   5,     19) =       3.83
Statistics robust to heteroskedasticity           Prob > F        =     0.0143
                                                  R-squared       =     0.6988
                                                  Adj R-squared   =     0.6577
                                                  Within R-sq.    =     0.3380
Number of clusters (cntry_id) =         20        Root MSE        =     4.4182

                                  (Std. err. adjusted for 20 clusters in cntry_id)
----------------------------------------------------------------------------------
                 |               Robust
npl_ratio_filled | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
-----------------+----------------------------------------------------------------
      gdp_growth |  -.0062792   .0827329    -0.08   0.940    -.1794412    .1668828
    unemployment |   1.250086   .4225684     2.96   0.008       .36564    2.134532
       inflation |   .2164479   .2286291     0.95   0.356    -.2620783    .6949742
L1_credit_growth |  -.0388332   .0607383    -0.64   0.530    -.1659598    .0882934
    rate_x_slack |   .0473949    .391186     0.12   0.905    -.7713668    .8661566
           _cons |  -5.421848   3.806264    -1.42   0.171    -13.38845    2.544754
----------------------------------------------------------------------------------

Absorbed degrees of freedom:
-----------------------------------------------------+
 Absorbed FE | Categories  - Redundant  = Num. Coefs |
-------------+---------------------------------------|
    cntry_id |        20          20           0    *|
        year |        21           1          20     |
-----------------------------------------------------+
* = FE nested within cluster; treated as redundant for DoF computation

. eststo INT_NPL

. 
. reghdfe capital_adequacy_ratio_filled gdp_growth unemployment inflation L1_credit_growth rate_x_slack, ///
>     absorb(cntry_id year) vce(cluster cntry_id)
(MWFE estimator converged in 5 iterations)

HDFE Linear regression                            Number of obs   =        367
Absorbing 2 HDFE groups                           F(   5,     19) =       5.51
Statistics robust to heteroskedasticity           Prob > F        =     0.0027
                                                  R-squared       =     0.8726
                                                  Adj R-squared   =     0.8552
                                                  Within R-sq.    =     0.1140
Number of clusters (cntry_id) =         20        Root MSE        =     1.7503

                                  (Std. err. adjusted for 20 clusters in cntry_id)
----------------------------------------------------------------------------------
                 |               Robust
capital_adequa~d | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
-----------------+----------------------------------------------------------------
      gdp_growth |   .0885476   .0529159     1.67   0.111    -.0222067    .1993018
    unemployment |  -.1736765   .0665004    -2.61   0.017    -.3128635   -.0344895
       inflation |  -.0959756   .1391605    -0.69   0.499    -.3872418    .1952906
L1_credit_growth |   -.069403   .0364872    -1.90   0.072    -.1457716    .0069655
    rate_x_slack |   .2561188   .1994709     1.28   0.215    -.1613785    .6736161
           _cons |     18.313   .8681359    21.09   0.000     16.49597    20.13003
----------------------------------------------------------------------------------

Absorbed degrees of freedom:
-----------------------------------------------------+
 Absorbed FE | Categories  - Redundant  = Num. Coefs |
-------------+---------------------------------------|
    cntry_id |        20          20           0    *|
        year |        21           1          20     |
-----------------------------------------------------+
* = FE nested within cluster; treated as redundant for DoF computation

. eststo INT_T1

. 
. esttab INT_Z INT_NPL INT_T1 using "out_tex/Table9_FE_interaction.tex", replace ///
>     label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
>     mtitles("log Z (FE+int.)" "NPL (FE+int.)" "Tier-1 (FE+int.)") ///
>     booktabs alignment(S) compress ///
>     addnotes("Country and year fixed effects included; SEs clustered by country.", ///
>              "Real short rate omitted in FE due to collinearity with year FE; interaction term shown as robustness c
> heck.")
(output written to out_tex/Table9_FE_interaction.tex)

. 
. * ============================================================
. * TABLES 10–12 — System-GMM (disciplined instruments)
. *  - Two-step, Windmeijer-robust (robust small)
. *  - Collapsed GMM, lag(2 3), time FE as 4-yr bins (IV in levels)
. *  - #instruments constrained (< #countries)
. *  - Correctly prints AR(2) and Hansen via e(m2p) and e(hansenp)
. * ============================================================
. 
. * Time FE as 4-year bins
. gen bin4 = floor((year-2000)/4) if inrange(year,2000,2022)

. label define bin4 0 "2000–2003" 1 "2004–2007" 2 "2008–2011" 3 "2012–2015" 4 "2016–2019" 5 "2020–2022"

. label values bin4 bin4

. 
. eststo clear

. 
. * ---------- Table 10: GMM for log Z ----------
. xtabond2 log_bank_z_score L.log_bank_z_score ///
>     gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap ///
>     i.bin4, ///
>     gmm(L.log_bank_z_score, lag(2 3) collapse) ///
>     ivstyle(gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap, eq(level)) ///
>     ivstyle(i.bin4, eq(level)) ///
>     twostep robust small
Favoring space over speed. To switch, type or click on mata: mata set matafavor speed, perm.
0b.bin4 dropped due to collinearity
Warning: Two-step estimated covariance matrix of moments is singular.
  Using a generalized inverse to calculate optimal weighting matrix for two-step estimation.
  Difference-in-Sargan/Hansen statistics may be negative.

Dynamic panel-data estimation, two-step system GMM
------------------------------------------------------------------------------
Group variable: cntry_id                        Number of obs      =       338
Time variable : year                            Number of groups   =        20
Number of instruments = 15                      Obs per group: min =         8
F(12, 19)     =    565.28                                      avg =     16.90
Prob > F      =     0.000                                      max =        20
-----------------------------------------------------------------------------------
                  |              Corrected
 log_bank_z_score | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
------------------+----------------------------------------------------------------
 log_bank_z_score |
              L1. |   .4459168   .2632303     1.69   0.107    -.1050306    .9968642
                  |
       gdp_growth |    .001869   .0079478     0.24   0.817     -.014766    .0185039
     unemployment |  -.0083068   .0171735    -0.48   0.634    -.0442514    .0276377
        inflation |  -.1031408    .057146    -1.80   0.087    -.2227487    .0164672
        real_rate |  -.1023124   .0520867    -1.96   0.064    -.2113311    .0067063
 L1_credit_growth |   .0099743   .0066172     1.51   0.148    -.0038756    .0238243
credit_to_gdp_gap |  -.0054096   .0097461    -0.56   0.585    -.0258085    .0149893
                  |
             bin4 |
       2004–2007  |  -.0559814   .1236243    -0.45   0.656      -.31473    .2027672
       2008–2011  |  -.2541825    .174077    -1.46   0.161    -.6185298    .1101648
       2012–2015  |  -.1682732   .1723962    -0.98   0.341    -.5291026    .1925562
       2016–2019  |  -.2258636    .181215    -1.25   0.228     -.605151    .1534237
       2020–2022  |  -.3382397   .1840134    -1.84   0.082    -.7233842    .0469047
                  |
            _cons |   1.792983   .7997214     2.24   0.037     .1191466    3.466819
-----------------------------------------------------------------------------------
Instruments for first differences equation
  GMM-type (missing=0, separate instruments for each period unless collapsed)
    L(2/3).L.log_bank_z_score collapsed
Instruments for levels equation
  Standard
    0b.bin4 1.bin4 2.bin4 3.bin4 4.bin4 5.bin4
    gdp_growth unemployment inflation real_rate L1_credit_growth
    credit_to_gdp_gap
    _cons
  GMM-type (missing=0, separate instruments for each period unless collapsed)
    DL.L.log_bank_z_score collapsed
------------------------------------------------------------------------------
Arellano-Bond test for AR(1) in first differences: z =  -1.47  Pr > z =  0.141
Arellano-Bond test for AR(2) in first differences: z =   0.15  Pr > z =  0.878
------------------------------------------------------------------------------
Sargan test of overid. restrictions: chi2(2)    =   1.37  Prob > chi2 =  0.503
  (Not robust, but not weakened by many instruments.)
Hansen test of overid. restrictions: chi2(2)    =   2.25  Prob > chi2 =  0.325
  (Robust, but weakened by many instruments.)

Difference-in-Hansen tests of exogeneity of instrument subsets:
  GMM instruments for levels
    Hansen test excluding group:     chi2(1)    =   0.05  Prob > chi2 =  0.819
    Difference (null H = exogenous): chi2(1)    =   2.20  Prob > chi2 =  0.138


. eststo GMM_Z

. 
. * ---------- Table 11: GMM for NPL ----------
. xtabond2 npl_ratio_filled L.npl_ratio_filled ///
>     gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap ///
>     i.bin4, ///
>     gmm(L.npl_ratio_filled, lag(2 3) collapse) ///
>     ivstyle(gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap, eq(level)) ///
>     ivstyle(i.bin4, eq(level)) ///
>     twostep robust small
Favoring space over speed. To switch, type or click on mata: mata set matafavor speed, perm.
0b.bin4 dropped due to collinearity
Warning: Two-step estimated covariance matrix of moments is singular.
  Using a generalized inverse to calculate optimal weighting matrix for two-step estimation.
  Difference-in-Sargan/Hansen statistics may be negative.

Dynamic panel-data estimation, two-step system GMM
------------------------------------------------------------------------------
Group variable: cntry_id                        Number of obs      =       367
Time variable : year                            Number of groups   =        20
Number of instruments = 15                      Obs per group: min =         9
F(12, 19)     =    742.99                                      avg =     18.35
Prob > F      =     0.000                                      max =        21
-----------------------------------------------------------------------------------
                  |              Corrected
 npl_ratio_filled | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
------------------+----------------------------------------------------------------
 npl_ratio_filled |
              L1. |   1.094673    .153917     7.11   0.000     .7725211    1.416825
                  |
       gdp_growth |  -.0836884   .0340769    -2.46   0.024    -.1550121   -.0123647
     unemployment |  -.0586581   .0868928    -0.68   0.508    -.2405268    .1232106
        inflation |  -.0457937   .1608944    -0.28   0.779    -.3825496    .2909623
        real_rate |  -.0649245   .1264629    -0.51   0.614    -.3296144    .1997654
 L1_credit_growth |   .0365498   .0272505     1.34   0.196    -.0204862    .0935858
credit_to_gdp_gap |   .0631125   .0325008     1.94   0.067    -.0049124    .1311374
                  |
             bin4 |
       2004–2007  |  -.3705574   .3616658    -1.02   0.318    -1.127533    .3864178
       2008–2011  |  -.1412155    .320173    -0.44   0.664    -.8113453    .5289143
       2012–2015  |  -.1506019   .5361532    -0.28   0.782    -1.272783    .9715796
       2016–2019  |  -1.338466     .70059    -1.91   0.071    -2.804818    .1278858
       2020–2022  |  -.9715475   .6812372    -1.43   0.170    -2.397393    .4542983
                  |
            _cons |    .595581   .5137546     1.16   0.261    -.4797198    1.670882
-----------------------------------------------------------------------------------
Instruments for first differences equation
  GMM-type (missing=0, separate instruments for each period unless collapsed)
    L(2/3).L.npl_ratio_filled collapsed
Instruments for levels equation
  Standard
    0b.bin4 1.bin4 2.bin4 3.bin4 4.bin4 5.bin4
    gdp_growth unemployment inflation real_rate L1_credit_growth
    credit_to_gdp_gap
    _cons
  GMM-type (missing=0, separate instruments for each period unless collapsed)
    DL.L.npl_ratio_filled collapsed
------------------------------------------------------------------------------
Arellano-Bond test for AR(1) in first differences: z =  -1.77  Pr > z =  0.078
Arellano-Bond test for AR(2) in first differences: z =   0.85  Pr > z =  0.394
------------------------------------------------------------------------------
Sargan test of overid. restrictions: chi2(2)    =  68.78  Prob > chi2 =  0.000
  (Not robust, but not weakened by many instruments.)
Hansen test of overid. restrictions: chi2(2)    =   5.17  Prob > chi2 =  0.076
  (Robust, but weakened by many instruments.)

Difference-in-Hansen tests of exogeneity of instrument subsets:
  GMM instruments for levels
    Hansen test excluding group:     chi2(1)    =   2.58  Prob > chi2 =  0.108
    Difference (null H = exogenous): chi2(1)    =   2.59  Prob > chi2 =  0.108


. eststo GMM_NPL

. 
. * ---------- Table 12: GMM for Tier-1 ----------
. xtabond2 capital_adequacy_ratio_filled L.capital_adequacy_ratio_filled ///
>     gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap ///
>     i.bin4, ///
>     gmm(L.capital_adequacy_ratio_filled, lag(2 3) collapse) ///
>     ivstyle(gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap, eq(level)) ///
>     ivstyle(i.bin4, eq(level)) ///
>     twostep robust small
Favoring space over speed. To switch, type or click on mata: mata set matafavor speed, perm.
0b.bin4 dropped due to collinearity
Warning: Two-step estimated covariance matrix of moments is singular.
  Using a generalized inverse to calculate optimal weighting matrix for two-step estimation.
  Difference-in-Sargan/Hansen statistics may be negative.

Dynamic panel-data estimation, two-step system GMM
------------------------------------------------------------------------------
Group variable: cntry_id                        Number of obs      =       367
Time variable : year                            Number of groups   =        20
Number of instruments = 15                      Obs per group: min =         9
F(12, 19)     =  56002.46                                      avg =     18.35
Prob > F      =     0.000                                      max =        21
-----------------------------------------------------------------------------------------------
                              |              Corrected
capital_adequacy_ratio_filled | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
------------------------------+----------------------------------------------------------------
capital_adequacy_ratio_filled |
                          L1. |   .9289203   .1207183     7.69   0.000     .6762541    1.181587
                              |
                   gdp_growth |  -.0335339   .0324693    -1.03   0.315     -.101493    .0344252
                 unemployment |  -.0162938   .0344049    -0.47   0.641     -.088304    .0557163
                    inflation |  -.1882183   .1186242    -1.59   0.129    -.4365015     .060065
                    real_rate |  -.1688665   .1306832    -1.29   0.212    -.4423896    .1046566
             L1_credit_growth |  -.0217379   .0195657    -1.11   0.280    -.0626894    .0192136
            credit_to_gdp_gap |   -.002228   .0080384    -0.28   0.785    -.0190525    .0145964
                              |
                         bin4 |
                   2004–2007  |  -.3773018   .3970288    -0.95   0.354    -1.208293    .4536891
                   2008–2011  |   .0236602   .4233549     0.06   0.956    -.8624319    .9097522
                   2012–2015  |   .5128581   .4690678     1.09   0.288     -.468912    1.494628
                   2016–2019  |  -.2222857   .8457198    -0.26   0.796    -1.992397    1.547826
                   2020–2022  |   .2787405   .9945492     0.28   0.782    -1.802875    2.360356
                              |
                        _cons |   1.975188   1.798827     1.10   0.286    -1.789801    5.740177
-----------------------------------------------------------------------------------------------
Instruments for first differences equation
  GMM-type (missing=0, separate instruments for each period unless collapsed)
    L(2/3).L.capital_adequacy_ratio_filled collapsed
Instruments for levels equation
  Standard
    0b.bin4 1.bin4 2.bin4 3.bin4 4.bin4 5.bin4
    gdp_growth unemployment inflation real_rate L1_credit_growth
    credit_to_gdp_gap
    _cons
  GMM-type (missing=0, separate instruments for each period unless collapsed)
    DL.L.capital_adequacy_ratio_filled collapsed
------------------------------------------------------------------------------
Arellano-Bond test for AR(1) in first differences: z =  -1.66  Pr > z =  0.098
Arellano-Bond test for AR(2) in first differences: z =   0.88  Pr > z =  0.377
------------------------------------------------------------------------------
Sargan test of overid. restrictions: chi2(2)    =   0.79  Prob > chi2 =  0.672
  (Not robust, but not weakened by many instruments.)
Hansen test of overid. restrictions: chi2(2)    =   1.50  Prob > chi2 =  0.473
  (Robust, but weakened by many instruments.)

Difference-in-Hansen tests of exogeneity of instrument subsets:
  GMM instruments for levels
    Hansen test excluding group:     chi2(1)    =   0.45  Prob > chi2 =  0.504
    Difference (null H = exogenous): chi2(1)    =   1.05  Prob > chi2 =  0.305


. eststo GMM_T1

. 
. * ---------- LaTeX exports (print N, #instr, AR(2) p, Hansen p) ----------
. * Note: xtabond2 stores N=e(N), j=e(j), m2p=e(m2p), hansenp=e(hansenp)
. 
. esttab GMM_Z using "out_tex/Table10_GMM_zscore.tex", replace ///
>     label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
>     mtitles("log Z (Sys-GMM)") ///
>     booktabs alignment(S) compress ///
>     stats(N j m2p hansenp, labels("Obs." "# instr." "AR(2) p" "Hansen p") fmt(0 0 3 3)) ///
>     addnotes("Two-step System-GMM with Windmeijer correction.", ///
>              "GMM: L.y lag(2–3), collapsed; controls as standard IVs.", ///
>              "Time FE: 4-year bins as IVs; instrument count constrained (< number of countries).")
(output written to out_tex/Table10_GMM_zscore.tex)

. 
. esttab GMM_NPL using "out_tex/Table11_GMM_npl.tex", replace ///
>     label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
>     mtitles("NPL (Sys-GMM)") ///
>     booktabs alignment(S) compress ///
>     stats(N j m2p hansenp, labels("Obs." "# instr." "AR(2) p" "Hansen p") fmt(0 0 3 3)) ///
>     addnotes("Two-step System-GMM with Windmeijer correction.", ///
>              "GMM: L.y lag(2–3), collapsed; controls as standard IVs.", ///
>              "Time FE: 4-year bins as IVs; instrument count constrained (< number of countries).")
(output written to out_tex/Table11_GMM_npl.tex)

. 
. esttab GMM_T1 using "out_tex/Table12_GMM_tier1.tex", replace ///
>     label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
>     mtitles("Tier-1 (Sys-GMM)") ///
>     booktabs alignment(S) compress ///
>     stats(N j m2p hansenp, labels("Obs." "# instr." "AR(2) p" "Hansen p") fmt(0 0 3 3)) ///
>     addnotes("Two-step System-GMM with Windmeijer correction.", ///
>              "GMM: L.y lag(2–3), collapsed; controls as standard IVs.", ///
>              "Time FE: 4-year bins as IVs; instrument count constrained (< number of countries).")
(output written to out_tex/Table12_GMM_tier1.tex)

. 
. * ============================================================
. * OPTIONAL ROBUSTNESS (keep on hand if asked in viva)
. * Tighter lag window L(2 2) for NPL to further discipline instruments
. * (often moves Hansen p upward while keeping signs)
. * ============================================================
. capture noisily {
.     xtabond2 npl_ratio_filled L.npl_ratio_filled ///
>         gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap ///
>         i.bin4, ///
>         gmm(L.npl_ratio_filled, lag(2 2) collapse) ///
>         ivstyle(gdp_growth unemployment inflation real_rate L1_credit_growth credit_to_gdp_gap, eq(level)) ///
>         ivstyle(i.bin4, eq(level)) ///
>         twostep robust small
Favoring space over speed. To switch, type or click on mata: mata set matafavor speed, perm.
0b.bin4 dropped due to collinearity
Warning: Two-step estimated covariance matrix of moments is singular.
  Using a generalized inverse to calculate optimal weighting matrix for two-step estimation.
  Difference-in-Sargan/Hansen statistics may be negative.

Dynamic panel-data estimation, two-step system GMM
------------------------------------------------------------------------------
Group variable: cntry_id                        Number of obs      =       367
Time variable : year                            Number of groups   =        20
Number of instruments = 14                      Obs per group: min =         9
F(12, 19)     =    245.99                                      avg =     18.35
Prob > F      =     0.000                                      max =        21
-----------------------------------------------------------------------------------
                  |              Corrected
 npl_ratio_filled | Coefficient  std. err.      t    P>|t|     [95% conf. interval]
------------------+----------------------------------------------------------------
 npl_ratio_filled |
              L1. |   1.215657   .0916491    13.26   0.000     1.023833    1.407481
                  |
       gdp_growth |  -.1788517   .0739254    -2.42   0.026    -.3335793    -.024124
     unemployment |  -.1144287   .1034804    -1.11   0.283    -.3310157    .1021583
        inflation |   .0839484    .222495     0.38   0.710    -.3817389    .5496358
        real_rate |   .0612278   .1614285     0.38   0.709    -.2766459    .3991015
 L1_credit_growth |   .0641392   .0245482     2.61   0.017     .0127592    .1155192
credit_to_gdp_gap |   .0394271   .0327036     1.21   0.243    -.0290224    .1078765
                  |
             bin4 |
       2004–2007  |  -.2053579   .4212823    -0.49   0.632    -1.087112     .676396
       2008–2011  |   .0323025   .3989149     0.08   0.936    -.8026359    .8672409
       2012–2015  |   .2596207   .9842331     0.26   0.795    -1.800403    2.319644
       2016–2019  |  -1.607602    .720683    -2.23   0.038    -3.116009   -.0991955
       2020–2022  |  -.7674056   .8523832    -0.90   0.379    -2.551464    1.016653
                  |
            _cons |   .4141019   .9726983     0.43   0.675    -1.621779    2.449983
-----------------------------------------------------------------------------------
Instruments for first differences equation
  GMM-type (missing=0, separate instruments for each period unless collapsed)
    L2.L.npl_ratio_filled collapsed
Instruments for levels equation
  Standard
    0b.bin4 1.bin4 2.bin4 3.bin4 4.bin4 5.bin4
    gdp_growth unemployment inflation real_rate L1_credit_growth
    credit_to_gdp_gap
    _cons
  GMM-type (missing=0, separate instruments for each period unless collapsed)
    DL.L.npl_ratio_filled collapsed
------------------------------------------------------------------------------
Arellano-Bond test for AR(1) in first differences: z =  -1.86  Pr > z =  0.062
Arellano-Bond test for AR(2) in first differences: z =   0.78  Pr > z =  0.437
------------------------------------------------------------------------------
Sargan test of overid. restrictions: chi2(1)    =  10.66  Prob > chi2 =  0.001
  (Not robust, but not weakened by many instruments.)
Hansen test of overid. restrictions: chi2(1)    =   4.38  Prob > chi2 =  0.036
  (Robust, but weakened by many instruments.)

Difference-in-Hansen tests of exogeneity of instrument subsets:
  GMM instruments for levels
    Hansen test excluding group:     chi2(0)    =   0.00  Prob > chi2 =      .
    Difference (null H = exogenous): chi2(1)    =   4.38  Prob > chi2 =  0.036

.     eststo GMM_NPL_L22
. 
.     esttab GMM_NPL GMM_NPL_L22 using "Table11_GMM_npl_alt.tex", replace ///
>         label b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
>         mlabels("NPL (GMM L2–3)" "NPL (GMM L2–2)") ///
>         nomtitles nonumbers drop(_cons) booktabs alignment(S) compress ///
>         stats(N j m2p hansenp, labels("Obs." "# instr." "AR(2) p" "Hansen p") fmt(0 0 3 3)) ///
>         addnotes("Two-step System-GMM (Windmeijer). GMM(L.y) instruments collapsed; lag windows shown in headers.", 
> ///
>                  "Time FE: 4-year bins as IVs; instrument counts kept below country count.")
(output written to Table11_GMM_npl_alt.tex)
. }

. 
. display as text "----- Done. LaTeX tables written to current directory. -----"
----- Done. LaTeX tables written to current directory. -----

. 
end of do-file

. 
