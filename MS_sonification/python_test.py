# Script to test out Python methods
# Sam Siljee
# 12 July 2024

# Add path for pandas install
import sys
sys.path.append(r"c:\Users\siljeesa\AppData\Roaming\Python\Python312\site-packages")

# Load the packages
import pandas as pd

#load in the test spectrum
GABA_spectrum = pd.read_csv('test_spectra/GABA_spectrum.csv')
