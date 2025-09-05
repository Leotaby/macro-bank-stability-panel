setup:
	python -m venv .venv && . .venv/bin/activate && pip install -r requirements.txt && pre-commit install

lint:
	ruff src tests

test:
	pytest

data:
	python src/macrobank/build_data.py

figures:
	python src/macrobank/make_figures.py

all: data figures lint test

.PHONY: setup lint test data figures all
