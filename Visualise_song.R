# Song visualisation
# Sam Siljee
# 28th January 2026

# Script to take a song as input, and the produce a series of frames to visualise the spectra of FFT

# Libraries
library(tuneR) # read in .wav files
library(dplyr) # piping and data manipulation utilities
library(ggplots) # Make the plots for the graphics
library(av) # Produce the resulting video

# Variables
song_file <- "spectral_1 - Band-o-rama-tronics.wav"
output_dir <- paste0(getwd(), "/video_frames/")
video_fps <- 20
frame_height <- 1080
frame_width <- 1920
col_l <- "blue"
col_r <- "gray"

# Load functions from MS_sonification project for plotting
source("../MS_sonification/functions.R")

# Load the song
song <- readWave(song_file)

# Get some useful variables from the song
sample_rate <- song@samp.rate
n_frames <- floor(length(song) / sample_rate * video_fps)
l_channel <- song@left
r_channel <- song@right

# Main loop through for each frame
for(i in 1:n_frames){
    # Define times for the frame
    start_point <- (i - 1) / video_fps * sample_rate
    end_point <- round(i / video_fps * sample_rate)
    
    # Filter and split audio signal
    l_signal <- l_channel[start_point : end_point]
    r_signal <- r_channel[start_point : end_point]
    
    # Make data.frames for double plots - using FFT
    l_data <- data.frame(mz = 1:length(l_signal), intensity = abs(fft(l_signal)))
    r_data <- data.frame(mz = 1:length(r_signal), intensity = abs(fft(l_signal)))
    
    # Make the double-plot
    double_plot(spectrum_left = l_data, spectrum_right = r_data, colour_1 = col_l, colour_2 = col_r)
    
    # Save the plot
    ggsave(paste0(output_dir, i, ".png"), width = frame_width, height = frame_height, units = "px")
}

# Put the plots together in video format
av_encode_video(
    paste0(output_dir, 1:n_frames, ".png"),
    "video.mp4",
    framerate = video_fps,
    audio = song_file
)
