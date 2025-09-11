.PHONY: setup data figures fe

setup:
\tpython -m venv .venv && . .venv/bin/activate && pip install -U pip && pip install -r requirements.txt

data:
\t. .venv/bin/activate && python src/macrobank/build_data.py

figures: data
\t. .venv/bin/activate && python src/macrobank/make_figures.py

fe: data
\t. .venv/bin/activate && python src/macrobank/fe_models.py
