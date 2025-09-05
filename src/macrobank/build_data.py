from pathlib import Path
import pandas as pd


def main():
    """Generate a tiny processed panel sample with synthetic data."""
    data = pd.DataFrame({
        "country": ["AT", "BE", "CY"],
        "year": [2000, 2000, 2000],
        "bank_z_score": [2.3, 2.1, 1.9],
        "npl_ratio": [3.4, 4.5, 5.6],
        "tier1_ratio": [9.5, 10.2, 11.1],
    })

    # Determine project root by going two levels up from this file (src/macrobank)
    project_root = Path(__file__).resolve().parents[2]
    out_dir = project_root / "data" / "processed"
    out_dir.mkdir(parents=True, exist_ok=True)
    out_path = out_dir / "panel_sample.csv"
    data.to_csv(out_path, index=False)
    print(f"Saved sample data to {out_path}")


if __name__ == "__main__":
    main()
