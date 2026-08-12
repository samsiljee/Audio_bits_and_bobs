# Script to test some variations on generating tones
# Sam Siljee
# 3rd May 2024

# Libraries
# Load the required packages
library(tuneR)
library(mzR)
library(dplyr)

# Source functions
source("functions.R")

# Load test spectra
spectrum_1 <- read.csv("test_spectra/spectrum_1.csv", header = TRUE)
spectrum_2 <- read.csv("test_spectra/spectrum_2.csv", header = TRUE)

# Testing out different options
# Basic tone
basic_tone_1 <- advanced_spectrum_to_tone(spectrum_1, duration = 2)
writeWave(basic_tone_1, file = "Produced clips/basic_tone_1.wav")

# Scale from 100 - 15K
scaled_tone_1 <- advanced_spectrum_to_tone(
  spectrum_1,
  duration = 2,
  scale = TRUE,
  scale_min = 100,
  scale_max = 15000
)
writeWave(scaled_tone_1, file = "Produced clips/scaled_tone_1.wav")

# Log2 transform (with scale, till I sort that out again)
log_transformed_tone_1 <- advanced_spectrum_to_tone(
  spectrum_1,
  duration = 2,
  scale = TRUE,
  scale_min = 120.994,
  scale_max = 1336.602,
  log_transform = TRUE
)
writeWave(log_transformed_tone_1, file = "Produced clips/log_transformed_tone_1.wav")

# Filter
for (i in seq(0.1, 0.9, by = 0.1)) {
  filtered_tone_1 <- advanced_spectrum_to_tone(
    spectrum_1,
    duration = 2,
    filter_mz = TRUE,
    filter_thershold = i
  )
  writeWave(filtered_tone_1, file = paste0("Produced clips/", i, "_filtered_tone_1.wav"))
}

# Reverse m/z values
reversed_tone_1 <- advanced_spectrum_to_tone(
  spectrum_1,
  duration = 2,
  reverse_mz = TRUE
)
writeWave(reversed_tone_1, file = "Produced clips/reversed_tone_1.wav")
