# Data to wavetable for Vital
# Sam Siljee
# 4th June 2026

# Libraries
library(tuneR) # Audio files
library(dplyr) # Data manipulation and piping
library(mzR) # MS data handling

# Load custom functions
source("functions.R")

# Load raw MS data
#ms_data <- openMSfile("Mix_TMT_F5_20241219181938.mzML")
ms_data <- openMSfile("test.mzML")

# Get metadata to identify spectrum with the most peaks
ms_header <- header(ms_data)

# Extract spectrum with the most peaks
top_spectrum <- peaks(ms_data, which(ms_header$peaksCount == max(ms_header$peaksCount)))

# Pick spectrum with 200 peaks
spectrum <- peaks(ms_data, 10672)

# Run function to make wavetable
wavetable_vector <- make_wavetable(
  as.data.frame(top_spectrum),
  bw_range = c(1, 1000),
  data_points = 2048,
  frames = 256,
  pad = 4,
  kernel_method = "gaussian") 

# Plot to check waveform
plot(1:length(wavetable_vector), wavetable_vector, type = "l")

# Check some individual wavecycles
plot(1:2048, wavetable_vector[1:2048], type = "l")
plot(1:2048, wavetable_vector[((255*2048)+1):((255*2048)+2048)], type = "l")
plot(1:2048, wavetable_vector[((100*2048)+1):((100*2048)+2048)], type = "l")

# Export to wavetable
wavetable <- Wave(round(wavetable_vector), samp.rate = 44100, bit = 16)

# Export directly to Vital directory
writeWave(wavetable, file = "C:/Users/Sam/Documents/Vital/User/Wavetables/wavetable_smooth_3.wav")

# Test for different kernel methods
kernel_methods <- c("gaussian", "epanechnikov", "rectangular",
                    "triangular", "biweight",
                    "cosine", "optcosine")

# Loop through different kernels
for(k in kernel_methods) {
  # Run function to make wavetable
  wavetable_vector <- make_wavetable(
    as.data.frame(top_spectrum),
    bw_range = c(1, 1000),
    data_points = 2048,
    frames = 256,
    pad = 4,
    kernel_method = k) 
  
  # Export to wavetable
  wavetable <- Wave(round(wavetable_vector), samp.rate = 44100, bit = 16)
  
  # Export directly to Vital directory
  writeWave(wavetable, file = paste0("C:/Users/Sam/Documents/Vital/User/Wavetables/", k, ".wav"))
}
