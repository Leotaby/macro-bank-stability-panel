import importlib

def test_import_macrobank():
    """Test that the macrobank package and modules import correctly."""
    macrobank = importlib.import_module("macrobank")
    assert hasattr(macrobank, "build_data") or hasattr(macrobank, "make_figures"), "Expected functions not found in macrobank module"
