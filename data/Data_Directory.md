# Data Directory

This repository organizes data into three subdirectories:

- **raw/**: Original raw datasets, which are NOT tracked in Git. To obtain the raw data, follow the instructions in the project documentation or contact the authors. Do not commit raw data files.
- **interim/**: Intermediate data used during processing or analysis. These may include cleaned or partially transformed versions of the raw data. They are excluded from version control but can be reproduced by running the pipeline scripts.
- **processed/**: Processed data ready for analysis or modeling. For demonstration, running `python src/macrobank/build_data.py` will create a sample `panel_sample.csv` in this directory.

Each of these directories includes a `.gitkeep` file to ensure the directory is present in the repository structure. Large binary files (e.g., CSV, XLSX, Parquet, PNG) are managed via Git LFS (see `.gitattributes`).
