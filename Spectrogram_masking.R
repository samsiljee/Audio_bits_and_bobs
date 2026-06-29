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
window_overlap <- round(window_size * 0.5)

# Define some functions
stft <- function(x, n, hop) {
  # Returns a complex matrix: rows = frequency bins, cols = time frames
  w <- hanning(n)
  nframes <- floor((length(x) - n) / hop) + 1
  S <- matrix(0 + 0i, nrow = n %/% 2 + 1, ncol = nframes)
  for (i in seq_len(nframes)) {
    start <- (i - 1) * hop + 1
    segment <- x[start:(start + n - 1)] * w
    full_fft <- fft(segment)
    S[, i] <- full_fft[1:(n %/% 2 + 1)]
  }
  S
}

istft <- function(S, n, hop, original_length) {
  # Overlap-add inverse STFT
  nframes <- ncol(S)
  w <- hanning(n)
  out <- numeric(original_length)
  win_sum <- numeric(original_length)

  for (i in seq_len(nframes)) {
    # Mirror spectrum and take real part of inverse FFT
    full_spec <- c(S[, i], Conj(rev(S[-c(1, nrow(S)), i])))
    segment <- Re(fft(full_spec, inverse = TRUE)) / n
    start <- (i - 1) * hop + 1
    end <- start + n - 1
    idx <- start:end
    out[idx] <- out[idx] + segment * w
    win_sum[idx] <- win_sum[idx] + w^2
  }

  # Normalise for window overlap
  nonzero <- win_sum > 1e-8
  out[nonzero] <- out[nonzero] / win_sum[nonzero]
  out
}

process_channel <- function(audio_vec, mask_vec, n, hop, intensity = 1) {
    S_audio <- stft(audio_vec, n, hop)
    S_mask  <- stft(mask_vec,  n, hop)
    
    audio_mag <- abs(S_audio)
    mask_mag  <- abs(S_mask)
    
    # Make a mask
    mask <- -1 * (mask_mag - max(mask_mag))
    
    # Apply mask, preserving phase
    S_out <- audio_mag * mask * S_audio
    
    # Convert back to audio signal
    istft(S_out, n, hop, length(audio_vec))
}

normalise <- function(audio_vec, bit = 16) {
    scale <- 2^(bit - 1) - 1
    as.integer(round(audio_vec / max(abs(audio_vec)) * scale))
}

# Import data
audio <- readWave(audio_file)
masking <- readWave(masking_file)

# Trim longer audio to the same length as the shorter one
min_length <- min(length(audio@left), length(masking@left))

# Extract audio vectors
audio_l <- as.numeric(audio@left[1:min_length])
audio_r <- as.numeric(audio@right[1:min_length])
masking_l <- as.numeric(masking@left[1:min_length])
masking_r <- as.numeric(masking@right[1:min_length])

# Process each channel
masked_l <- process_channel(audio_l, masking_l, window_size, window_overlap, intensity = 0.5)
masked_r <- process_channel(audio_r, masking_r, window_size, window_overlap, intensity = 0.5)

# Export masked audio
Wave(
  left = normalise(masked_l, audio@bit),
  right = normalise(masked_r, audio@bit),
  samp.rate = audio@samp.rate,
  bit = audio@bit
) %>%
  writeWave(file = paste0("masked_", audio_file))

