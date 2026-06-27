# Spectral masking
# Sam Siljee
# 26th June 2026

# Project to take an audio track, and mask it using a second audio track

# Libraries
library(dplyr) # Data manipulation
library(tuneR) # Read and write audio
library(signal) # Make spectrograms

# Set some variables
audio_file <- "drone.wav"
masking_file <- "spectral_3_no_drone.wav"
window_size <- 1024
window_overlap <- 0.5

# Import data
audio <- readWave(audio_file)
masking <- readWave(masking_file)

# Trim longer audio to the same length as the shorter one
min_length <- min(c(length(audio), length(masking)))
audio <- audio[1:min_length]
masking <- masking[1:min_length]

# Use FFT to transform to spectrogram
audio_l <- abs(specgram(audio@left, n = window_size, overlap = window_overlap)$S)
audio_r <- abs(specgram(audio@right, n = window_size, overlap = window_overlap)$S)
masking_l <- abs(specgram(masking@left, n = window_size, overlap = window_overlap)$S)
masking_r <- abs(specgram(masking@right, n = window_size, overlap = window_overlap)$S)

# Take inverse of masking spectrogram
mask_l <- 1/masking_l
mask_r <- 1/masking_r

# Multiply input audio by inverse masking spectrogram
masked_l <- audio_l * mask_l
masked_r <- audio_r * mask_r

# Revert from spectrogram back to waveform

# Export masked audio
# Write back to .wav file
Wave(
    left = round(masked_l),
    right = round(masked_r),
    samp.rate = audio@samp.rate,
    bit = audio@bit
) %>%
    normalize(unit = as.character(audio@bit), center = TRUE, level = 1, rescale = TRUE) %>%
    writeWave(file = paste0("masked_", audio_file))

