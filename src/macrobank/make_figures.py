# src/macrobank/make_figures.py
from __future__ import annotations
import pandas as pd
import numpy as np
from pathlib import Path
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parents[2]
DATA = ROOT / "data" / "processed"
FIGS = ROOT / "figures"
FIGS.mkdir(parents=True, exist_ok=True)

PANEL = DATA / "panel_sample.csv"

# (title, ylabel, filename, variable)
PLOTS = [
    ("EA median + IQR — log Z-score", "log Z", "figure_5a_logZ.png", "logz"),
    ("EA median + IQR — NPL ratio (%)", "percent", "figure_5b_NPL.png", "npl"),
    ("EA median + IQR — Tier-1 capital (%)", "capital", "figure_5c_Capital.png", "capital"),
    ("EA median + IQR — Real GDP growth (%)", "percent", "figure_6a_GDPg.png", "gdp_g"),
    ("EA median + IQR — Unemployment (%)", "percent", "figure_6b_Unemp.png", "unemp"),
    ("EA median + IQR — Inflation (%)", "percent", "figure_6c_Inflation.png", "infl"),
    ("EA median + IQR — Real short rate (pp)", "pp", "figure_6d_RealRate.png", "real_rate"),
    ("EA median + IQR — L1 Credit growth (%)", "percent", "figure_6e_L1CreditGrowth.png", "l1_cred_g"),
    ("EA median + IQR — Credit-to-GDP gap (pp)", "pp", "figure_6f_CreditGap.png", "credgap"),
]

def median_iqr(df, var):
    g = df.groupby("year")[var]
    med = g.median()
    q25 = g.quantile(0.25)
    q75 = g.quantile(0.75)
    return pd.DataFrame({"median": med, "q25": q25, "q75": q75})

def plot_series(stats: pd.DataFrame, title: str, ylabel: str, outpath: Path):
    years = stats.index.values
    plt.figure(figsize=(10,4))
    plt.fill_between(years, stats["q25"], stats["q75"], alpha=0.2)
    plt.plot(years, stats["median"], linewidth=2)
    plt.title(title)
    plt.ylabel(ylabel)
    plt.xlabel("Year")
    plt.grid(alpha=0.3, linestyle="--")
    plt.tight_layout()
    plt.savefig(outpath, dpi=200)
    plt.close()

def main():
    df = pd.read_csv(PANEL)
    for title, ylabel, fname, var in PLOTS:
        if var not in df.columns:
            print(f"[skip] {var} not in panel; skipping {fname}")
            continue
        stats = median_iqr(df, var)
        out = FIGS / fname
        plot_series(stats, title, ylabel, out)
        print(f"[figures] Wrote {out}")

if __name__ == "__main__":
    main()
