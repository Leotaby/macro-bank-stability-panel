#!/usr/bin/env python3
"""
Figure 4 — Missingness heatmap (with imputation flags)

Creates a 2-panel figure:
  (A) Availability heatmap: % of EA-20 countries with non-missing values by year × variable
  (B) Imputation heatmap: % of available observations flagged as imputed (for vars with flags)

Inputs (expected in --data-dir, defaults to /mnt/data):
  - Euro_B_stability_master.csv        # primary panel
  - master_real_rate.csv               # real short rate; if missing cells, compute policy - inflation
  - T1_FE_Specs_bank_z_score.csv       # optional, to include log Z coverage
  - codebook.csv                       # optional, not required

Outputs:
  - Figure4_Missingness.png  (and .svg if --save-svg)

Usage:
  python figure4_missingness.py --data-dir /path/to/data --out-dir ./figures --save-svg
"""

import argparse
import os
from typing import Dict, Tuple, List, Optional

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.colors import ListedColormap

# -----------------------
# Utility: robust column name resolution
# -----------------------

def pick_first(cols: List[str], candidates: List[str]) -> Optional[str]:
    for c in candidates:
        if c in cols:
            return c
    return None

def load_master(data_dir: str) -> pd.DataFrame:
    path = os.path.join(data_dir, "Euro_B_stability_master.csv")
    if not os.path.exists(path):
        raise FileNotFoundError(f"Could not find {path}")
    df = pd.read_csv(path)
    return df

def load_real_rate(data_dir: str) -> pd.DataFrame:
    path = os.path.join(data_dir, "master_real_rate.csv")
    if not os.path.exists(path):
        # Return empty frame; we will compute fallback if needed
        return pd.DataFrame()
    rr = pd.read_csv(path)
    return rr

def load_z_slice(data_dir: str) -> pd.DataFrame:
    # Optional: include coverage for (log) Z if the slice exists
    path = os.path.join(data_dir, "T1_FE_Specs_bank_z_score.csv")
    if not os.path.exists(path):
        return pd.DataFrame()
    z = pd.read_csv(path)
    return z

def standardize_keys(df: pd.DataFrame) -> Tuple[pd.DataFrame, str, str]:
    cols = df.columns.tolist()
    country_key = pick_first(cols, ["country", "country_id", "iso3", "iso", "country_code"])
    year_key = pick_first(cols, ["year", "Year", "YEAR"])
    if country_key is None or year_key is None:
        raise ValueError(f"Could not identify country/year keys in columns: {cols}")
    return df, country_key, year_key

# -----------------------
# Build working dataset
# -----------------------

def build_working_panel(data_dir: str) -> Tuple[pd.DataFrame, str, str]:
    master = load_master(data_dir)
    master, ckey, ykey = standardize_keys(master)

    # Merge real rate if present; else compute fallback = policy rate - inflation if possible
    rr = load_real_rate(data_dir)
    if not rr.empty:
        rr, ckey_rr, ykey_rr = standardize_keys(rr)
        # Try to find real rate column
        rr_col = pick_first(rr.columns.tolist(), ["real_rate", "real_short_rate", "real_policy_rate"])
        if rr_col is None:
            # Allow file but no column: we'll compute from policy - inflation after merge
            rr_col = None
        merged = master.merge(rr, left_on=[ckey, ykey], right_on=[ckey_rr, ykey_rr], how="left", suffixes=("", "_rr"))
        # Try to populate real rate
        # Candidate columns in master for policy & inflation
        policy_col = pick_first(merged.columns.tolist(), ["policy_rate", "ecb_refi_rate", "policy"])
        infl_col   = pick_first(merged.columns.tolist(), ["inflation", "infl", "hicp_inflation"])
        if rr_col and rr_col in merged.columns:
            merged["real_short_rate_fig4"] = merged[rr_col]
        elif policy_col and infl_col:
            merged["real_short_rate_fig4"] = merged[policy_col] - merged[infl_col]
        else:
            merged["real_short_rate_fig4"] = np.nan
    else:
        merged = master.copy()
        # Compute fallback inside master if columns exist
        policy_col = pick_first(merged.columns.tolist(), ["policy_rate", "ecb_refi_rate", "policy"])
        infl_col   = pick_first(merged.columns.tolist(), ["inflation", "infl", "hicp_inflation"])
        if policy_col and infl_col:
            merged["real_short_rate_fig4"] = merged[policy_col] - merged[infl_col]
        else:
            merged["real_short_rate_fig4"] = np.nan

    return merged, ckey, ykey

# -----------------------
# Variable map (friendly label -> (column_name, imputation_flag_or_None))
# -----------------------

def make_variable_map(df: pd.DataFrame) -> Dict[str, Tuple[Optional[str], Optional[str]]]:
    cols = df.columns.tolist()

    def has(name_list): return pick_first(cols, name_list)

    varmap = {
        "Tier-1 capital ratio":         (has(["capital_adequacy_ratio_filled", "tier1_ratio_filled", "tier1_ratio"]), has(["imputed_tier1", "imputed_tier_1"])),
        "NPL ratio":                    (has(["npl_ratio_filled", "npl_ratio", "npls"]), has(["imputed_npl", "imputed_npls"])),
        "GDP growth":                   (has(["gdp_growth", "gdp_g", "rgdp_growth"]), None),
        "Unemployment":                 (has(["unemployment", "unemp_rate"]), None),
        "Inflation":                    (has(["inflation", "infl", "hicp_inflation"]), None),
        "Real short rate":              ("real_short_rate_fig4", None),  # already created
        "Credit growth":                (has(["credit_growth", "cred_growth", "credit_g"]), None),
        "Credit-to-GDP gap":            (has(["credit_to_gdp_gap", "credit_gap", "credit_gdp_gap"]), None),
    }
    return varmap

def maybe_add_logz(data_dir: str, df: pd.DataFrame, ckey: str, ykey: str, varmap: Dict[str, Tuple[Optional[str], Optional[str]]]) -> Dict[str, Tuple[Optional[str], Optional[str]]]:
    z = load_z_slice(data_dir)
    if z.empty:
        return varmap
    z, c2, y2 = standardize_keys(z)
    # Try to find log-Z column
    zcol = pick_first(z.columns.tolist(), ["log_z", "logZ", "ln_z", "lnZ", "bank_z_score", "z_score"])
    if zcol is None:
        return varmap
    # Merge a thin coverage indicator so we can compute availability
    z_small = z[[c2, y2, zcol]].copy()
    z_small.rename(columns={c2: ckey, y2: ykey, zcol: "logz_for_coverage"}, inplace=True)
    df = df.merge(z_small, on=[ckey, ykey], how="left")
    # Place it back (we’ll just refer to column name "logz_for_coverage")
    varmap = {"log Z-score": ("logz_for_coverage", None), **varmap}
    return varmap

# -----------------------
# Coverage & imputation matrices
# -----------------------

def coverage_by_year(df: pd.DataFrame, ckey: str, ykey: str, varmap: Dict[str, Tuple[Optional[str], Optional[str]]]) -> Tuple[pd.DataFrame, pd.DataFrame, List[int], List[str]]:
    years = sorted(df[ykey].dropna().unique().astype(int).tolist())
    labels = []
    avail_mat = []
    imput_mat = []

    # Identify country set (EA-20)
    countries = sorted(df[ckey].dropna().unique().tolist())
    n_c = len(countries)

    for label, (col, flag) in varmap.items():
        if col is None or col not in df.columns:
            # Skip variables not present
            continue

        row_avail = []
        row_imput = []

        for yr in years:
            sl = df[df[ykey] == yr]
            # availability share
            avail = sl[col].notna().mean() if len(sl) > 0 else np.nan
            row_avail.append(100.0 * avail)

            # imputation share among available
            if flag and flag in sl.columns:
                denom = sl[col].notna().sum()
                if denom > 0:
                    imputed = sl.loc[sl[col].notna(), flag].fillna(0).astype(int).mean() * 100.0
                else:
                    imputed = np.nan
            else:
                imputed = np.nan
            row_imput.append(imputed)

        labels.append(label)
        avail_mat.append(row_avail)
        imput_mat.append(row_imput)

    A = pd.DataFrame(avail_mat, index=labels, columns=years)
    I = pd.DataFrame(imput_mat, index=labels, columns=years)
    return A, I, years, labels

# -----------------------
# Plotting
# -----------------------

def plot_figure4(A: pd.DataFrame, I: pd.DataFrame, years: List[int], labels: List[str], out_path_png: str, save_svg: bool):
    fig_w = max(12, int(len(years) * 0.35))
    fig_h = max(6, int(len(labels) * 0.8))
    fig, axes = plt.subplots(1, 2, figsize=(fig_w, fig_h), constrained_layout=True)

    # Panel A: Availability heatmap (%)
    ax0 = axes[0]
    im0 = ax0.imshow(A.values, aspect="auto", interpolation="nearest", vmin=0, vmax=100)
    ax0.set_title("Availability (% of EA-20 with non-missing values)")
    ax0.set_yticks(np.arange(len(labels)))
    ax0.set_yticklabels(labels)
    ax0.set_xticks(np.arange(len(years)))
    ax0.set_xticklabels(years, rotation=90)
    cbar0 = fig.colorbar(im0, ax=ax0, fraction=0.046, pad=0.04)
    cbar0.set_label("% available")

    # Annotate low coverage tiles (< 80%) to make thin spots obvious
    for i in range(A.shape[0]):
        for j in range(A.shape[1]):
            val = A.values[i, j]
            if np.isfinite(val) and val < 80:
                ax0.text(j, i, f"{val:.0f}", ha="center", va="center", fontsize=7, color="black")

    # Panel B: Imputed share (% of available), only where flags exist
    ax1 = axes[1]
    # Replace variables without flags with NaN so they appear blank
    I_plot = I.copy()
    # Build a mask: if entire row is NaN, leave as is; else heatmap 0-100
    im1 = ax1.imshow(I_plot.values, aspect="auto", interpolation="nearest", vmin=0, vmax=100)
    ax1.set_title("Imputed share among available (%)")
    ax1.set_yticks(np.arange(len(labels)))
    ax1.set_yticklabels(labels)
    ax1.set_xticks(np.arange(len(years)))
    ax1.set_xticklabels(years, rotation=90)
    cbar1 = fig.colorbar(im1, ax=ax1, fraction=0.046, pad=0.04)
    cbar1.set_label("% imputed (within available)")

    # Annotate imputation percentages where >= 5%
    for i in range(I_plot.shape[0]):
        for j in range(I_plot.shape[1]):
            val = I_plot.values[i, j]
            if np.isfinite(val) and val >= 5:
                ax1.text(j, i, f"{val:.0f}", ha="center", va="center", fontsize=7, color="black")

    # Overall title
    fig.suptitle("Figure 4 — Missingness heatmap (coverage and imputation flags)", fontsize=14, y=1.02)

    # Save
    os.makedirs(os.path.dirname(out_path_png), exist_ok=True)
    fig.savefig(out_path_png, dpi=300, bbox_inches="tight")
    if save_svg:
        fig.savefig(out_path_png.replace(".png", ".svg"), bbox_inches="tight")
    plt.close(fig)

# -----------------------
# Main
# -----------------------

def main():
    parser = argparse.ArgumentParser(description="Build Figure 4 (Missingness heatmap)")
    parser.add_argument("--data-dir", default="/mnt/data", help="Directory containing input CSVs")
    parser.add_argument("--out-dir", default=".", help="Directory to save outputs")
    parser.add_argument("--save-svg", action="store_true", help="Also save an SVG")
    args = parser.parse_args()

    df, ckey, ykey = build_working_panel(args.data_dir)

    varmap = make_variable_map(df)
    varmap = maybe_add_logz(args.data_dir, df, ckey, ykey, varmap)

    A, I, years, labels = coverage_by_year(df, ckey, ykey, varmap)

    out_path_png = os.path.join(args.out_dir, "Figure4_Missingness.png")
    plot_figure4(A, I, years, labels, out_path_png, save_svg=args.save_svg)

    # Console summary
    print("Figure 4 written to:", out_path_png)
    print("\nAvailability summary (last 5 years):")
    print(A[years[-5:]].round(1).to_string())

    # Note which variables lacked flags
    flagged = []
    unflagged = []
    for lbl, (_, flag) in varmap.items():
        if lbl not in A.index:  # skipped due to missing column
            continue
        (flagged if flag is not None else unflagged).append(lbl)
    if flagged:
        print("\nVariables with imputation flags included in Panel B:", ", ".join(flagged))
    if unflagged:
        print("Variables without imputation flags (blank in Panel B):", ", ".join(unflagged))

if __name__ == "__main__":
    main()
