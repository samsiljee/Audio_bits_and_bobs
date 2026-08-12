# Script to sonify an entire mass spectrometry run - TMT
# Sam Siljee
# 11 September 2024
# MIT licence

# Directories
directory <- "C:/Users/sam.siljee/OneDrive - GMRI/Documents/Coding/MS_sonification"
setwd(directory)

# Libraries
library(tuneR) # Audio processing
library(mzR) # MS data handling
library(dplyr) # Piping and utilities

# Source functions
source("functions.R")

# Set some variables
sample_rate <- 44100
duration <- 1

# Load and extract data
ms_data <- openMSfile("Steph_TMTpro-16plex_DEC2021_B2_TR3_HpHF_F3.mzML")
ms_peaks <- peaks(ms_data)
ms_header <- header(ms_data)

# Get list of MS1 and MS2 spectrum indexes and rows
ms1_indexes <- filter(ms_header, msLevel == 1) %>% .$seqNum
ms2_indexes <- filter(ms_header, msLevel == 2) %>% .$seqNum
ms3_indexes <- filter(ms_header, msLevel == 3) %>% .$seqNum

# Synthesise waveforms, and write to .txt (so big runs can be interrupted)
for (i in ms_header$seqNum) {
  write(
    spectrum_to_waveform(ms_peaks[[i]], duration = duration, sampling_rate = sample_rate),
    ncolumns = 1,
    file = paste0("TMT_waveforms/waveform_", i, ".txt")
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
  waveform <- read.delim(paste0("TMT_waveforms/waveform_", i, ".txt"), header = FALSE)[, 1] * log(ms_header$totIonCurrent[i])

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
  file = paste0("TMT_waveforms/ms1_compiled_log_waveforms.txt")
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
  waveform <- read.delim(paste0("TMT_waveforms/waveform_", i, ".txt"), header = FALSE)[, 1] * log(ms_header$totIonCurrent[i])

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
  file = paste0("TMT_waveforms/ms2_compiled_log_waveforms.txt")
)

# Repeat for MS3 scans
# Initialise blank waveform
ms3_waveform <- rep(0, total_time + duration * sample_rate)

# Read in the .txt files
for (i in ms3_indexes) {
  # Generate blank time before and after the waveform
  RT_pre <- rep(0, round(sample_rate * (ms_header$retentionTime[i] - min_RT)))
  RT_post <- rep(0, total_time - length(RT_pre))
  
  # read in the waveform and account for intensity
  waveform <- read.delim(paste0("TMT_waveforms/waveform_", i, ".txt"), header = FALSE)[, 1] * log(ms_header$totIonCurrent[i])
  
  # Add to the waveform with blank RT sound
  ms3_waveform <- ms3_waveform + c(RT_pre, waveform, RT_post)
  
  # Monitor progress
  print(i)
}

# Normalise the waveform
ms3_waveform <- (ms3_waveform / max(abs(ms3_waveform))) * 32000

# Write the MS2 compiled waveform to disk
write(
  ms3_waveform,
  ncolumns = 1,
  file = paste0("TMT_waveforms/ms3_compiled_log_waveforms.txt")
)

# Create multi-channel wav object
multi_channel_wave_obj <- Wave(
  data = data.frame(
    MS1 = ms1_waveform,
    MS2 = ms2_waveform,
    MS3 = ms3_waveform
  ),
  samp.rate = sample_rate,
  bit = 16
)

# Save as .wav
writeWave(multi_channel_wave_obj, file = "Produced clips/good/Multi_channel_log_TMT.wav")
