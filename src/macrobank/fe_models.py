# src/macrobank/fe_models.py
from __future__ import annotations
import pandas as pd
from pathlib import Path
import statsmodels.formula.api as smf

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "data" / "processed"
TABLES = ROOT / "tables"
TABLES.mkdir(parents=True, exist_ok=True)

PANEL = DATA / "panel_sample.csv"

# Baseline RHS as in your thesis
BASE_X = ["gdp_g", "unemp", "infl", "real_rate", "l1_cred_g", "credgap"]

def run_fe(df: pd.DataFrame, y: str):
    # Two-way FE via country/year dummies; cluster by country
    rhs = " + ".join([x for x in BASE_X if x in df.columns])
    formula = f"{y} ~ {rhs} + C(country) + C(year)"
    model = smf.ols(formula, data=df).fit(
        cov_type="cluster",
        cov_kwds={"groups": df["country"]}
    )
    return model

def main():
    df = pd.read_csv(PANEL).dropna(subset=["country","year"])
    df = df.sort_values(["country","year"])
    outcomes = [c for c in ["logz","npl","capital"] if c in df.columns]

    rows = []
    for y in outcomes:
        res = run_fe(df, y)
        coefs = res.params.to_frame("coef")
        ses = res.bse.to_frame("se")
        out = coefs.join(ses)
        out.insert(0, "outcome", y)
        rows.append(out.reset_index().rename(columns={"index":"term"}))

        # Save a compact table too
        summ = {
            "outcome": y,
            "N": int(res.nobs),
            "R2": res.rsquared,
            "adj_R2": res.rsquared_adj
        }
        print(f"[FE] {y}: N={summ['N']} R2={summ['R2']:.3f}")

    long_table = pd.concat(rows, ignore_index=True)
    long_table.to_csv(TABLES / "Table8_FE_Parsimonious.csv", index=False)
    print(f"[tables] Wrote {TABLES/'Table8_FE_Parsimonious.csv'}")

if __name__ == "__main__":
    main()
