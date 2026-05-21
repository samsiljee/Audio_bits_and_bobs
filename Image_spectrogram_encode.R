# Script to encode an image as a audio file with the image in the spectrogram
# Sam Siljee
# 10th May 2026

# Load some libraries
library(dplyr) # Data manipulation
library(png) # Read in PNG data
library(tiff) # Read in TIFF data
library(jpeg) # Read in JPEG data
library(tuneR) # Export the audio clip
library(stringr) # File name change

# Define function to encode an image in an audio spectrogram
# file_path: path to image to encode
# sample_rate: sample rate of audio output
# duration: length of produced audio clip in seconds
# freq_range: frequencies to scale the image height over
# freq_scaling: method to distribute image rows as frequencies, accepts "log" and "linear" currently, plan to include more

# Define the function
encode_spectrogram <- function(file_path,
                               sample_rate = 44100,
                               duration = 10,
                               freq_range = c(50, 20000),
                               freq_scaling = "log") {
  # Get image extension
  extension <- str_extract(file_path, "\\.[^.]+$")

  # Read in the image data
  switch(extension,
    .png = {
      image_data <- readPNG(file_path)
    },
    .tif = {
      image_data <- readTIFF(file_path)
    },
    .jpg = {
      image_data <- readJPEG(file_path)
    }
  )

  # Reduce to 2D greyscale, mean RGB channels
  image_data <- (image_data[, , 1] + image_data[, , 2] + image_data[, , 3]) / 3

  # Pre-calculate some variables
  n_rows <- nrow(image_data)
  n_samples <- sample_rate * duration

  # Frequency vector
  switch(freq_scaling,
    log = {
      freq_vector <- exp(seq(log(freq_range[2]), log(freq_range[1]), length.out = n_rows))
    },
    linear = {
      freq_vector <- seq(freq_range[2], freq_range[1], length.out = n_rows)
    }
  )

  # Time vector
  time_seq <- seq(0, duration, length.out = n_samples)

  # Initialise blank vector for sine waves
  audio_vector <- numeric(n_samples)

  # Loop through image rows and frequency vector
  for (i in seq_len(n_rows)) {
    if (i %% 10 == 0) {
      cat(sprintf("Progress: %d%%\n", round(i / n_rows * 100)))
    }

    # Interpolate pixel row to audio length
    pixel_intensities <- approx(x = image_data[i, ], n = n_samples, method = "constant")$y

    # Accumulate: amplitude-modulated sine wave added in place
    audio_vector <- audio_vector + pixel_intensities * sin(2 * pi * freq_vector[i] * time_seq)
  }

  # Normalise vector
  audio_vector <- (audio_vector / max(abs(audio_vector))) * 32000

  # Export audio
  audio_wav <- Wave(round(audio_vector), samp.rate = sample_rate, bit = 16)
  writeWave(audio_wav, file = str_replace(file_path, extension, ".wav"))
}

# Run the function to get audio
# Set some variables
encode_spectrogram(
  file_path = "ULTRASOUND OBSTETRIC ANATOMY - Set 3 - Image 63.png",
  sample_rate = 44100,
  duration = 3*60,
  freq_range = c(50, 20000),
  freq_scaling = "log"
)
