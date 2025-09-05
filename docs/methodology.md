# Methods (concise)

## Two-way Fixed Effects
We estimate:
\[
y_{c,t} = \beta' X_{c,t} + \mu_c + \tau_t + \varepsilon_{c,t}
\]
with entity (country) and time (year) dummies. Demo code uses
`statsmodels` OLS with dummy matrices. Clustered SEs by country are trivialized
for the small sample; for research runs, prefer a robust cluster estimator.

## Dynamic panel (System-GMM) — stubbed
We outline:
\[
y_{c,t} = \rho y_{c,t-1} + \beta' X_{c,t} + \mu_c + \tau_t + \varepsilon_{c,t}
\]
Use `linearmodels` DynamicPanelGMM with collapsed instruments and limited lag depth
as discussed in the thesis. The repo keeps an interface spot but does not run GMM
in CI to keep it fast and resource-light.
