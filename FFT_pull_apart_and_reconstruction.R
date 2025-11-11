# Script where I read in the audio, deconstruct with FFT into component parts, then reconstruct back to audio signal. FOr use as a template for how to use the FFT for other scripts
# Sam Siljee
# 11th November 2025

# Libraries
library(tuneR) # Audio manipulation
library(dplyr) # Data manipulation and piping

file_path <- "test.wav"

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
phases <- Arg(fft_result)

# Get frequencies
frequencies <- 0:(n-1) / n * audio_data@samp.rate

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
    writeWave(file = paste0("reconstructed_", file_path))
