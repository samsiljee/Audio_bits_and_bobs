# Script to encode an image as a audio file with the image in the spectrogram
# Sam Siljee
# 10th May 2026

# Load some libraries
library(dplyr) # Data manipulation
library(png) # Read in PNG data
library(tuneR) # Export the audio clip

# Set some variables
file_path <- "ULTRASOUND OBSTETRIC ANATOMY - Set 3 - Image 1.png" # Image to convert
sample_rate <- 44100 # Audio sample rate
duration <- 1 # Duration of the clip in seconds
freq_range <- c(50, 20000) # Frequencies to use in the audio

# Read in the image data
image_data <- readPNG(file_path)

# Reduce to 2D greyscale, Sum RGB channels
image_data <- image_data[,,1] + image_data[,,2] + image_data[,,3]

# Use frequency range and image height to make vector of frequencies
freq_vector <- seq(freq_range[2], freq_range[1], length.out = nrow(image_data))

# Initialise blank matrix
audio_matrix <- matrix(data = NA, nrow = nrow(image_data), ncol = sample_rate * duration)

# Loop through image rows and frequency vector
for(i in 1:nrow(image_data)){
    # Vector of pixel intensities
    pixel_intensities <- image_data[i,]
    
    # Sine wave vector at the right frequency
    time_seq <- seq(0, freq_vector[i] * pi, length.out = sample_rate * duration)
    sine_vector <- sin(time_seq)
    
    # Multiply by pixel intensities and add to audio matrix
    audio_matrix[i,] <- pixel_intensities * sine_vector
}

# Get column sums
audio_vector <- colSums(audio_matrix)

# Normalise vector
audio_vector <- (audio_vector / max(abs(audio_vector))) * 32000

# Export audio
audio_wav <- Wave(round(audio_vector), samp.rate = sample_rate, bit = 16)
writeWave(audio_wav, file = "Image_spectrogram.wav")
