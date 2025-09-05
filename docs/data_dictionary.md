# Data Dictionary

This data dictionary summarizes the key variables in the processed panel sample. The table below lists each variable with its unit and primary source. Detailed descriptions follow in the text.

| Variable | Unit | Source |
| --- | --- | --- |
| `bank_z_score` | unitless | Constructed from bank-level ROA and equity-to-asset ratios |
| `npl_ratio` | % | IMF Financial Soundness Indicators |
| `capital_adequacy_ratio` | % | IMF FSI / supervisory reports |
| `gdp_growth` | % | IMF World Economic Outlook / World Bank WDI |
| `unemployment` | % | World Bank WDI / Eurostat |
| `inflation` | % | IMF WEO / Eurostat |
| `real_rate` | pp | Policy rate minus inflation (ECB / national data) |
| `credit_growth` | % | Annual growth of credit to the private sector (BIS/national statistics) |
| `credit_to_gdp_gap` | pp | Credit-to-GDP ratio minus long-run trend (BIS credit gap) |

## Variable descriptions

- **bank_z_score**: The Z-score measures the number of standard deviations by which returns must fall to deplete equity; higher values indicate more stability.
- **npl_ratio**: The ratio of non-performing loans to total loans, indicating asset-quality stress.
- **capital_adequacy_ratio**: Tier-1 capital to risk-weighted assets, a risk-weighted solvency buffer.
- **gdp_growth**: Real GDP growth rate.
- **unemployment**: Unemployment rate.
- **inflation**: Price inflation (headline or GDP deflator).
- **real_rate**: Real short-term interest rate (policy rate minus inflation).
- **credit_growth**: Annual growth of credit to the private sector (typically lagged one period in models).
- **credit_to_gdp_gap**: Deviation of credit-to-GDP ratio from its long-run trend; positive gaps signal leverage booms.

Continuous variables are usually demeaned within country and may be lagged before entering regressions. Imputation flags (e.g., `imputed_npl`, `imputed_tier1`) accompany series with missing observations.
