# Script to distort sounds by realigning the phase
# Sam Siljee
# 11 November 2025

# Libraries
library(tuneR) # Audio manipulation
library(dplyr) # Data manipulation and piping

file_path <- "test.wav"
transient_click <- FALSE
distortion_ratio <- 1

# Load data from .wav file
audio_data <- readWave(file_path)

# Extract as numeric vector
mono_data <- audio_data@left

# Perform the FFT
fft_result <- fft(mono_data)

# Get lengths for vectors
n <- length(fft_result)

# Get magnitudes
magnitudes <- Mod(fft_result)

# Get phases
real_phases <- Arg(fft_result)

# Make fake phases - using transient click if desired
distorted_phases <- if(transient_click) {
    rep(0, n)
} else {
    1:n * 0.618034
}

# Combine real with distorted phases at ratio determined
phases <- distorted_phases * distortion_ratio + real_phases * (1 - distortion_ratio)

# Reconstruct FFT series
fft_reconstructed <- magnitudes * complex(real = cos(phases), im = sin(phases))

# Reconstruct signal
mono_data_reconstructed <- Re(fft(fft_reconstructed, inverse = TRUE) / n)

# Write back to .wav file
Wave(
    round(mono_data_reconstructed),
    samp.rate = audio_data@samp.rate,
    bit = audio_data@bit
) %>%
    normalize(unit = as.character(audio_data@bit), center = TRUE, level = 1, rescale = TRUE) %>%
    writeWave(file = paste0("phase_shifted_", file_path))
