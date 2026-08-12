# Script to sonify an entire mass spectrometry run - version for DIA test data
# Sam Siljee
# 23 August 2024
# MIT licence

# Directories
directory <- "C:/Users/sam.siljee/OneDrive - GMRI/Documents/Coding/MS_sonification"
setwd(directory)

# Libraries
library(tuneR)
library(mzR)
library(dplyr)

# Source functions
source("functions.R")

# Set some variables
sample_rate <- 44100
duration <- 1

# Load and extract data
ms_data <- openMSfile("meningothelial_cell_08.mzML")
ms_peaks <- peaks(ms_data)
ms_header <- header(ms_data)

# Get list of MS1 and MS2 spectrum indexes and rows
ms1_indexes <- filter(ms_header, msLevel == 1) %>% .$seqNum
ms2_indexes <- filter(ms_header, msLevel == 2) %>% .$seqNum

# Synthesise waveforms, and write to .txt (so big runs can be interrupted)
for (i in ms_header$seqNum) {
  write(
spectrum_to_waveform(ms_peaks[[i]], duration = duration, sampling_rate = sample_rate),
    ncolumns = 1,
    file = paste0("DIA_waveforms/waveform_", i, ".txt")
  )
}

# Total time of the clip, at set sample rate
max_RT <- max(ms_header$retentionTime)
min_RT <- min(ms_header$retentionTime)
total_time <- round(sample_rate * (max_RT - min_RT + 1))

# Make a stereo compilation, Left channel for MS1 spectra, Right channel for MS2 spectra
# Initialise blank waveform
ms1_waveform <- rep(0, total_time + duration * sample_rate)

# Read in the .txt files and add to the waveform
for (i in ms1_indexes) {
  # Generate blank time before and after the waveform
  RT_pre <- rep(0, round(sample_rate * (ms_header$retentionTime[i] - min_RT)))
  RT_post <- rep(0, total_time - length(RT_pre))

  # read in the waveform and account for intensity
  waveform <- read.delim(paste0("DIA_waveforms/waveform_", i , ".txt"), header = FALSE)[, 1] * log(ms_header$totIonCurrent[i])

  # Add to the waveform with blank RT sound
  ms1_waveform <- ms1_waveform + c(RT_pre, waveform, RT_post)

  # Monitor progress
  print(i)
}

# Normalise the waveform
ms1_waveform <- (ms1_waveform / max(abs(ms1_waveform))) * 32000

# Write the MS1 compiled waveform to disk
write(
  ms1_waveform,
  ncolumns = 1,
  file = paste0("DIA_waveforms/ms1_compiled_log_waveforms.txt")
)

# Repeat for MS2 scans
# Initialise blank waveform
ms2_waveform <- rep(0, total_time + duration * sample_rate)

# Read in the .txt files
for (i in ms2_indexes) {
  # Generate blank time before and after the waveform
  RT_pre <- rep(0, round(sample_rate * (ms_header$retentionTime[i] - min_RT)))
  RT_post <- rep(0, total_time - length(RT_pre))

  # read in the waveform and account for intensity
  waveform <- read.delim(paste0("DIA_waveforms/waveform_", i, ".txt"), header = FALSE)[, 1] * log(ms_header$totIonCurrent[i])

  # Add to the waveform with blank RT sound
  ms2_waveform <- ms2_waveform + c(RT_pre, waveform, RT_post)

  # Monitor progress
  print(i)
}

# Normalise the waveform
ms2_waveform <- (ms2_waveform / max(abs(ms2_waveform))) * 32000

# Write the MS2 compiled waveform to disk
write(
  ms2_waveform,
  ncolumns = 1,
  file = paste0("DIA_waveforms/ms2_compiled_log_waveforms.txt")
)

# Create stereo wav object
stereo_wave_obj <- Wave(
  left = round(ms1_waveform),
  right = round(ms2_waveform),
  samp.rate = sample_rate,
  bit = 16
)

# Save as .wav
writeWave(stereo_wave_obj, file = "Produced clips/good/Log_test_whole_stereo_DIA.wav")
