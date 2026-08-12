# Script to sonify test tree spectra
# Sam Siljee
# 1/10/24

# Libraries
library(tuneR) # Making audio files
library(dplyr) # Pipe

# Source functions
source("functions.R")

# Set some variables
input_dir <- "Tree_data"
output_dir <- "Produced clips/Tree_tones"

# List files
files_list <- list.files(input_dir)

# Initialise list to load the spectra
spectrum_list <- as.list(files_list)

# Read in the data
for(i in 1:length(spectrum_list)){
  # Read in data
  dat <- read.delim(
    paste(input_dir, files_list[i], sep = "/"),
    sep = " ",
    header = FALSE)
  # Name columns
  colnames(dat) <- c("mz", "intensity")
  # Add to list
  spectrum_list[[i]] <- dat
}

# Create standard tones
for(i in 1:length(spectrum_list)){
  # Generate tone
  tone <- advanced_spectrum_to_tone(
    spectrum = spectrum_list[[i]],
    duration = 5
  )
  file_name <- paste(
    output_dir,
    paste0(strtrim(files_list[i], nchar(files_list[i]) - 4), ".wav"),
    sep = "/"
    )
  # Save as audio clip
  writeWave(tone, file = file_name)
}

# Reverse m/z tones
for(i in 1:length(spectrum_list)){
  # Generate tone
  tone <- advanced_spectrum_to_tone(
    spectrum = spectrum_list[[i]],
    duration = 5,
    reverse_mz = TRUE
  )
  file_name <- paste(
    output_dir,
    paste0(strtrim(files_list[i], nchar(files_list[i]) - 4), "_mz_reverse.wav"),
    sep = "/"
  )
  # Save as audio clip
  writeWave(tone, file = file_name)
}

# Linear scale
for(i in 1:length(spectrum_list)){
  # Generate tone
  tone <- advanced_spectrum_to_tone(
    spectrum = spectrum_list[[i]],
    duration = 5,
    scale = TRUE
  )
  file_name <- paste(
    output_dir,
    paste0(strtrim(files_list[i], nchar(files_list[i]) - 4), "_linear_scale_100_15k.wav"),
    sep = "/"
  )
  # Save as audio clip
  writeWave(tone, file = file_name)
}

# Log scale
for(i in 1:length(spectrum_list)){
  # Generate tone
  tone <- advanced_spectrum_to_tone(
    spectrum = spectrum_list[[i]],
    duration = 5,
    scale = TRUE,
    log_transform_mz = TRUE
  )
  file_name <- paste(
    output_dir,
    paste0(strtrim(files_list[i], nchar(files_list[i]) - 4), "_log_scale_100_15k.wav"),
    sep = "/"
  )
  # Save as audio clip
  writeWave(tone, file = file_name)
}
