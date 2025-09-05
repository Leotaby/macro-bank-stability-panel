# Macro Bank Stability Panel

Research code for EA‑20 (2000‑2022) macro→bank‑stability analysis. It ships a reproducible **mini pipeline** plus a baseline **two‑way FE** implementation, and CI for quality.

## What's here

- **Data mini‑pipeline** (`src/macrobank/build_data.py`) → `data/processed/panel_sample.csv` (3 rows).  
- **Figures** (`src/macrobank/make_figures.py`) → `figures/bank_z_sample.png`.  
- **Baseline FE** (`src/macrobank/fe_baseline.py`) – simple two‑way FE via `statsmodels` that saves `tables/table8_fe.csv` + `tables/table8_fe.md` from the sample panel.  
- **Docs**: `docs/data_dictionary.md`, `docs/methodology.md`, `data/README.md`.  
- **Quality**: `ruff`, `pytest`, `pre‑commit`; GitHub Actions runs lint + tests.

> Dynamic **System‑GMM** is outlined and stubbed; the small demo data keeps CI fast.  
> Drop in your full EA‑20 panel later and the same interfaces will work.

## Quickstart

Clone the repo and run the following commands:

```bash
make setup       # install dependencies, set up pre-commit
make data        # build the sample processed CSV
make figures     # produce a demo figure
make fe          # fit simple two‑way FE on the sample, saving Table 8 outputs
make test        # run pytest including pipeline output checks
make lint        # run ruff for code quality
make all         # run data + figures + fe + lint + test
```

Outputs after `make all`:

- `data/processed/panel_sample.csv`
- `figures/bank_z_sample.png`
- `tables/table8_fe.csv`, `tables/table8_fe.md`

## Methods snapshot

- **Two‑way fixed effects (FE)**: Ordinary Least Squares on entity (country) and time (year) dummies, clustered by country (simple HC1 at demo scale).  
- **Dynamic panel (System‑GMM)** – stubbed: interface in place; enable with `linearmodels` and your real panel (see `docs/methodology.md`).

## Data policy

- `data/raw/` and `data/interim/` are **excluded from Git** (see `.gitignore`).  
- Only small synthetic samples and derivative artifacts may be tracked or committed (or via LFS).  
- See `data/README.md` for how to obtain source data and where processed files are stored.

## License & citation

- MIT License (see `LICENSE`).  
- See `CITATION.cff` for citation format.

For more details on variables and methods, consult the files in the `docs/` directory.
