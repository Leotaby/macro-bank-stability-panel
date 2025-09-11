# src/macrobank/build_data.py
from __future__ import annotations
import pandas as pd
import numpy as np
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "data"
PROCESSED = DATA / "processed"
PROCESSED.mkdir(parents=True, exist_ok=True)

# Candidate inputs already in your repo root
CANDIDATES = [
    ROOT / "Euro_B_stability_enhanced.csv",
    ROOT / "Euro_B_stability_with_controls.csv",
    ROOT / "Euro_B_stability_master.csv",
]

# Columns we expect/standardize
RENAME_MAP = {
    "iso2": "country",
    "Country": "country",
    "YEAR": "year",
}
EXPECTED = [
    "country","year",
    "logz","npl","capital",
    "gdp_g","unemp","infl",
    "real_rate","cred_g","credgap"
]

def load_first_available() -> pd.DataFrame:
    for p in CANDIDATES:
        if p.exists():
            df = pd.read_csv(p)
            # Normalize column names
            df.columns = [c.strip().lower() for c in df.columns]
            df = df.rename(columns={k.lower(): v for k, v in RENAME_MAP.items() if k.lower() in df.columns})

            # If capital_adequacy_ratio_filled is present, use it as 'capital' (Tier-1)
            if "capital_adequacy_ratio_filled" in df.columns and "capital" not in df.columns:
                df = df.rename(columns={"capital_adequacy_ratio_filled": "capital"})

            # Keep only expected cols that exist
            cols = [c for c in EXPECTED if c in df.columns]
            df = df[cols].copy()

            # Minimal checks
            if not {"country","year"}.issubset(df.columns):
                raise ValueError(f"Missing country/year in {p.name}. Found columns: {df.columns.tolist()}")

            return df
    raise FileNotFoundError("None of the candidate CSVs were found in repo root.")

def add_lags(df: pd.DataFrame) -> pd.DataFrame:
    df = df.sort_values(["country","year"])
    if "cred_g" in df.columns and "l1_cred_g" not in df.columns:
        df["l1_cred_g"] = df.groupby("country")["cred_g"].shift(1)
    return df

def main():
    df = load_first_available()
    df = add_lags(df)

    # Basic cleaning: drop rows without core outcomes
    core_outcomes = ["logz","npl","capital"]
    keep = df.dropna(subset=[c for c in core_outcomes if c in df.columns])
    out = keep.copy()

    out_path = PROCESSED / "panel_sample.csv"
    out.to_csv(out_path, index=False)
    print(f"[build_data] Wrote {out_path} with {len(out)} rows and {len(out.columns)} columns.")

if __name__ == "__main__":
    main()
