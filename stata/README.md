# Stata replication files

Run in this order:

1. `Stata_Baseline.do` - Two-way FE baseline (log Z, NPL, Tier-1)
2. `Stata_Baseline_FE_GMM.do` - FE + System-GMM baseline
3. `STATA_Baseline_FE_GMM_AR_HSN.do` - GMM with AR and Hansen diagnostics
4. `STATA_Baseline_FE_GMM_AR_HSN2.do` - GMM robustness variant

Data required: `Euro_B_stability_master*.csv`, `master_real_rate.csv`
Sources: GFDD/IMF FSI, Eurostat, ECB SDW, BIS credit series
