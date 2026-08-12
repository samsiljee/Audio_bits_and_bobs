# Data to wavetable for Vital
# Sam Siljee
# 4th June 2026

# Libraries
library(tuneR) # Audio files
library(dplyr) # Data manipulation and piping
library(png) # Load in image
library(pracma) # interp2 call in the make_image_wavetable function

# Load custom functions
source("functions.R")

# Read in image
image_data <- readPNG("ULTRASOUND OBSTETRIC ANATOMY - Set 3 - Image 63.png")

# Reduce to 2D greyscale, mean RGB channels
image_matrix <- (image_data[, , 1] + image_data[, , 2] + image_data[, , 3]) / 3

# Call function to turn image into a wavetable
wavetable_vector <- make_image_wavetable(
    image_matrix,
    data_points = 2048,
    frames = 256,
    row_wise = FALSE)

# Check some individual wavecycles
plot(1:2048, wavetable_vector[1:2048], type = "l")
plot(1:2048, wavetable_vector[((255*2048)+1):((255*2048)+2048)], type = "l")
plot(1:2048, wavetable_vector[((100*2048)+1):((100*2048)+2048)], type = "l")

# Export to wavetable
wavetable <- Wave(round(wavetable_vector), samp.rate = 44100, bit = 16)

# Export directly to Vital directory
writeWave(wavetable, file = "C:/Users/Sam/Documents/Vital/User/Wavetables/image_wavetable.wav")

# Convert three-channel IF image to three wavetables
# Read in image
image_data_IF <- readPNG("IF_RGB.png")

# Reduce to 2D greyscale, mean RGB channels
IF_R <- image_data_IF[, , 1]
IF_G <- image_data_IF[, , 2]
IF_B <- image_data_IF[, , 3]

# Call function to turn image into a wavetable
wavetable_IF_R <- make_image_wavetable(IF_R)
wavetable_IF_G <- make_image_wavetable(IF_G)
wavetable_IF_B <- make_image_wavetable(IF_B)

# Export to wavetable
wavetable_R <- Wave(round(wavetable_IF_R), samp.rate = 44100, bit = 16)
wavetable_G <- Wave(round(wavetable_IF_G), samp.rate = 44100, bit = 16)
wavetable_B <- Wave(round(wavetable_IF_B), samp.rate = 44100, bit = 16)

# Export directly to Vital directory
writeWave(wavetable_R, file = "C:/Users/Sam/Documents/Vital/User/Wavetables/IF_R.wav")
writeWave(wavetable_G, file = "C:/Users/Sam/Documents/Vital/User/Wavetables/IF_G.wav")
writeWave(wavetable_B, file = "C:/Users/Sam/Documents/Vital/User/Wavetables/IF_B.wav")
