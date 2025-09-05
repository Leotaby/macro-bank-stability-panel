from pathlib import Path
import pandas as pd
import matplotlib.pyplot as plt


def main():
    """Generate a simple figure from the processed sample panel."""
    project_root = Path(__file__).resolve().parents[2]
    data_path = project_root / "data" / "processed" / "panel_sample.csv"
    if not data_path.exists():
        raise FileNotFoundError(f"{data_path} not found. Run build_data.py first.")

    data = pd.read_csv(data_path)
    fig, ax = plt.subplots()
    # Plot Z-score by country using sample data
    ax.plot(data["country"], data["bank_z_score"], marker="o")
    ax.set_xlabel("Country")
    ax.set_ylabel("Z-score")
    ax.set_title("Sample Bank Z-score by Country")
    fig.tight_layout()

    out_dir = project_root / "figures"
    out_dir.mkdir(parents=True, exist_ok=True)
    fig_path = out_dir / "bank_z_sample.png"
    fig.savefig(fig_path)
    print(f"Saved figure to {fig_path}")


if __name__ == "__main__":
    main()
