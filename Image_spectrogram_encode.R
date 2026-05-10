# Script to encode an image as a audio file with the image in the spectrogram
# Sam Siljee
# 10th May 2026

# Load some libraries
library(dplyr) # Data manipulation
library(png) # Read in PNG data
library(tuneR) # Export the audio clip
library(stringr) # File name change

# Set some variables
file_path <- "ULTRASOUND OBSTETRIC ANATOMY - Set 3 - Image 63.png" # Image to convert
sample_rate <- 44100 # Audio sample rate
duration <- 8 # Duration of the clip in seconds
freq_range <- c(50, 20000) # Frequencies to use in the audio
log_freq <- TRUE # Use linear or log frequencies

# Read in the image data
image_data <- readPNG(file_path)

# Reduce to 2D greyscale, mean RGB channels
image_data <- (image_data[,,1] + image_data[,,2] + image_data[,,3]) / 3

# Use frequency range and image height to make vector of frequencies
if(log_freq){ # Use log frequencies
    freq_vector <- exp(seq(log(freq_range[2]), log(freq_range[1]), length.out = nrow(image_data)))
} else { # Use linear frequencies
    freq_vector <- seq(freq_range[2], freq_range[1], length.out = nrow(image_data))
}

# Make time vector
time_seq <- seq(0, duration, length.out = sample_rate * duration)

# Initialise blank matrix for sine waves
audio_matrix <- matrix(data = NA, nrow = nrow(image_data), ncol = sample_rate * duration)

# Loop through image rows and frequency vector
for(i in 1:nrow(image_data)){
    # Vector of pixel intensities
    pixel_intensities <- spline(x = image_data[i,], n = sample_rate * duration)$y
    
    # Sine wave vector at the right frequency
    sine_vector <- sin(2 * pi * freq_vector[i] * time_seq)
    
    # Multiply by pixel intensities and add to audio matrix
    audio_matrix[i,] <- pixel_intensities * sine_vector
}

# Get column sums
audio_vector <- colSums(audio_matrix)

# Normalise vector
audio_vector <- (audio_vector / max(abs(audio_vector))) * 32000

# Export audio
audio_wav <- Wave(round(audio_vector), samp.rate = sample_rate, bit = 16)
writeWave(audio_wav, file = str_replace(file_path, ".png", ".wav"))
