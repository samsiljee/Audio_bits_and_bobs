# Script to make some tones for the Spectral_1 song
# Sam Siljee
# 12th July 2025

# Libraries
library(tuneR) # Audio processing
library(mzR) # MS data handling
library(dplyr) # Data manipulation

# Source functions
source("functions.R")

# Set some variables
sample_rate <- 44100

# Load raw MS data
ms_data <- openMSfile("Mix_TMT_F5_20241219181938.mzML")

# Get metadata to identify spectrum with the most peaks
ms_header <- header(ms_data)

# Extract spectrum with the most peaks
top_spectrum <- peaks(ms_data, which(ms_header$peaksCount == max(ms_header$peaksCount)))

tone <- advanced_spectrum_to_tone(
  spectrum = top_spectrum,
  duration = (7*60),
  scale = TRUE,
  scale_min = 100,
  scale_max = 15000,
  waveform = "sine"
)

# Save as audio clip
writeWave(tone, file = "tones/triangle.wav")
