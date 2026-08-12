# Script to make some tones for the Spectral_1 song
# Sam Siljee
# 12th July 2025

# Libraries
library(tuneR) # Audio processing
library(mzR) # MS data handling
library(dplyr) # Data manipulation

# Source functions
source("MS_sonification/functions.R")

# Set some variables
sample_rate <- 44100
duration <- 1

Interesting_proteins <- c(
  "P25705",
  "Q8N5M1",
  "P06576",
  "Q5TC12",
  "P36542",
  "P30049",
  "P56381",
  "P48047", # ATP synthase subunits
  "P14780", # MMP-9
  "Q05397", # Focal adhesion kinase
  "P16104" # H2AX
)

# Load the PSM data
load("MS_sonification/PSMs.rda")

data <- dplyr::filter(PSMs, Master.Protein.Accessions %in% Interesting_proteins)

# Find the run with the most IDs
data %>%
  group_by(Spectrum.File) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Identified "Mix_TMT_F5_20241219181938.raw" as having the most PSMs matching. Check which proteins are identified in this run
data %>% dplyr::filter(
  Spectrum.File == "Mix_TMT_F5_20241219181938.raw" &
    Master.Protein.Accessions %in% Interesting_proteins
) %>%
  .$Master.Protein.Accessions %>%
  unique

# Filter PSM data to just those we're interested in getting tones from
selected_PSMs <- data %>% dplyr::filter(
  Spectrum.File == "Mix_TMT_F5_20241219181938.raw" &
    Master.Protein.Accessions %in% Interesting_proteins
)

# Load raw MS data
ms_data <- openMSfile("MS_sonification/Mix_TMT_F5_20241219181938.mzML")
ms_peaks <- peaks(ms_data)

# Generate tones
for (i in 1:length(selected_PSMs)) {
  # Generate tone
  tone <- advanced_spectrum_to_tone(
    spectrum = ms_peaks[[selected_PSMs$First.Scan[i]]],
    duration = 5
  )
  file_name <- paste0("MS_sonification/tones/", selected_PSMs$Master.Protein.Accessions[i], "_", selected_PSMs$First.Scan[i], ".wav")
  
  # Save as audio clip
  writeWave(tone, file = file_name)
}

# extract the chromatogram
chr <- ProtGenerics::chromatogram(ms_data) %>% .[[1]]

# Create audio from chromatogram
chromatogram_audio <- tonify_chromatogram(chr$intensity)

# Save chromatogram audio
writeWave(chromatogram_audio, file = "MS_sonification/tones/chromatogram.wav")

# Select favourite tones
favourite_scans <- c(
  "59432",
  "42149",
  "30242",
  "47617",
  "61399",
  "45744",
  "16733",
  "52773"
)

# Filter for favourite PSMs
favourite_PSMs <- selected_PSMs %>% dplyr::filter(
  First.Scan %in% favourite_scans
)

# Generate longer tones for favourites
for (i in 1:length(favourite_PSMs)) {
  # Generate tone
  tone <- advanced_spectrum_to_tone(
    spectrum = ms_peaks[[favourite_PSMs$First.Scan[i]]],
    duration = 60
  )
  file_name <- paste0("MS_sonification/tones/long_", favourite_PSMs$Master.Protein.Accessions[i], "_", favourite_PSMs$First.Scan[i], ".wav")
  
  # Save as audio clip
  writeWave(tone, file = file_name)
}
