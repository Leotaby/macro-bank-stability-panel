# Figure 5: EA-20 medians with IQR bands for stability outcomes (log Z, NPL, Tier-1)
# - Reads the uploaded master panel
# - Computes per-year across-country median (line) and IQR (25–75%, shaded)
# - Produces three separate charts (no subplots), saved to /mnt/data/

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

# Load master panel
master = pd.read_csv("/mnt/data/Euro_B_stability_master.csv")
assert "country" in master.columns and "year" in master.columns

# Harmonize outcome columns
m = master.copy()
m = m.rename(columns={
    "capital_adequacy_ratio_filled": "tier1",
    "npl_ratio_filled": "npl"
})

# Compute log Z from raw bank_z_score (drop nonpositive/NaN safely)
if "bank_z_score" not in m.columns:
    raise ValueError("bank_z_score not found in the master file.")
m["log_z"] = np.where(m["bank_z_score"] > 0,
                      np.log(m["bank_z_score"].astype(float)),
                      np.nan)

def q_by_year(df, value_col):
    """Per-year across-country quantiles (25/50/75)."""
    g = df.dropna(subset=[value_col]).groupby("year")[value_col]
    q = g.quantile([0.25, 0.5, 0.75]).unstack(level=1).sort_index()
    q.columns = ["q25", "median", "q75"]
    return q

# Compute quantiles for each outcome
q_logz = q_by_year(m, "log_z")
q_npl  = q_by_year(m, "npl")
q_t1   = q_by_year(m, "tier1")

def plot_iqr(qdf, title, ylab, outfile):
    """Make a single chart with median line and IQR band."""
    years = qdf.index.values.astype(int)
    fig, ax = plt.subplots(figsize=(8, 4.5))
    ax.plot(years, qdf["median"].values, label="EA-20 median")
    ax.fill_between(years, qdf["q25"].values, qdf["q75"].values,
                    alpha=0.25, label="IQR (25–75%)")
    ax.set_title(title)
    ax.set_xlabel("Year")
    ax.set_ylabel(ylab)
    ax.grid(True, alpha=0.3)
    ax.legend()
    fig.tight_layout()
    fig.savefig(outfile, dpi=300, bbox_inches="tight")
    plt.close(fig)

# Output paths
out_dir = Path("/mnt/data")
plot_iqr(q_logz, "Figure 5a. EA-20 log Z — median and IQR (2000–2022)",
         "log Z-score", out_dir / "Figure5a_logZ.png")
plot_iqr(q_npl,  "Figure 5b. EA-20 NPL ratio — median and IQR (2000–2022)",
         "NPL (% of loans)", out_dir / "Figure5b_NPL.png")
plot_iqr(q_t1,   "Figure 5c. EA-20 Tier-1 capital ratio — median and IQR (2000–2022)",
         "Tier-1 ratio (%)", out_dir / "Figure5c_Tier1.png")
