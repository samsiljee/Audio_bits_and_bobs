# Song visualisation
# Sam Siljee
# 28th January 2026

# Script to take a song as input, and the produce a series of frames to visualise the spectra of FFT

# Libraries
library(tuneR) # read in .wav files
library(dplyr) # piping and data manipulation utilities
library(ggplot2) # Make the plots for the graphics
library(av) # Produce the resulting video

# Variables
song_file <- "spectral_1 - Band-o-rama-tronics.wav"
output_dir <- paste0(getwd(), "/video_frames/")
video_fps <- 20
frame_height <- 1080
frame_width <- 1920
# col_l <- "blue"
# col_r <- "gray"

# Load functions from MS_sonification project for plotting
source("../MS_sonification/functions.R")

# Load the song
song <- readWave(song_file)

# Get some useful variables from the song
sample_rate <- song@samp.rate
n_frames <- floor(length(song) / sample_rate * video_fps)
l_channel <- song@left
r_channel <- song@right

# Loop through time segments to run FFT and get contrast value
# Intialise variables
contrast <- numeric()
fft_data <- data.frame()

# Loop through time segments
progress <- 0
for(i in 1:n_frames){
    # Define times for the frame
    start_point <- round((i - 1) / video_fps * sample_rate) + 1
    end_point <- round(i / video_fps * sample_rate)
    signal_length <- end_point - start_point + 1
    
    # Filter and split audio signal
    l_signal <- l_channel[start_point : end_point]
    r_signal <- r_channel[start_point : end_point]
    
    # Get numerical range and add to contrast vector
    contrast <- c(
        contrast,
        (max(l_signal) + max(r_signal)) - (min(l_signal) + min(r_signal))
    )
    
    # Run FFT and add to the data.frame
    fft_data <- bind_rows(
        fft_data,
        data.frame(
            mz = 1:signal_length,
            intensity = abs(fft(l_signal)),
            channel = "l",
            frame = i)
    )
    fft_data <- bind_rows(
        fft_data,
        data.frame(
            mz = 1:signal_length,
            intensity = abs(fft(r_signal)),
            channel = "r",
            frame = i)
    )
    
    # Update progress
    if(progress != round(i*100/n_frames)){
        progress <- round(i*100/n_frames)
        print(paste0("Progress: ", progress, "%"))
    }
}

# Scale contrast vector
contrast <- contrast / max(contrast)

# Make colour vector from FFT data
# Filter segments for L/R RGB sections
# Initialise some vectors
col_l_r <- vector()
col_l_g <- vector()
col_l_b <- vector()
col_r_r <- vector()
col_r_g <- vector()
col_r_b <- vector()

# Loop through time segments to add to colour vectors
progress <- 0
for(i in 1:n_frames){
    # Define times for the frame
    start_point <- round((i - 1) / video_fps * sample_rate) + 1
    end_point <- round(i / video_fps * sample_rate)
    signal_length <- end_point - start_point + 1
    
    # Add left channel red value
    col_l_r <- c(
        col_l_r,
        mean(
            select(filter(fft_data, channel == "l" & frame == i), intensity)[1:(1/6*signal_length)]
        )
    )
    
    # Add left channel green value
    col_l_g <- c(
        col_l_g,
        mean(
            select(filter(fft_data, channel == "l" & frame == i), intensity)[(1/6*signal_length):(2/6*signal_length)]
        )
    )
    
    # Add left channel blue value
    col_l_b <- c(
        col_l_b,
        mean(
            select(filter(fft_data, channel == "l" & frame == i), intensity)[(2/6*signal_length):(3/6*signal_length)]
        )
    )
    
    # Add right channel red value
    col_r_r <- c(
        col_r_r,
        mean(
            select(filter(fft_data, channel == "r" & frame == i), intensity)[(3/6*signal_length):(4/6*signal_length)]
        )
    )
    
    # Add right channel green value
    col_r_g <- c(
        col_r_g,
        mean(
            select(filter(fft_data, channel == "r" & frame == i), intensity)[(4/6*signal_length):(5/6*signal_length)]
        )
    )
    
    # Add right channel blue value
    col_r_b <- c(
        col_r_b,
        mean(
            select(filter(fft_data, channel == "r" & frame == i), intensity)[(5/6*signal_length):signal_length]
        )
    )
    
    # Update progress
    if(progress != round(i*100/n_frames)){
        progress <- round(i*100/n_frames)
        print(paste0("Progress: ", progress, "%"))
    }
}

# Normalise and convert to 8-bit
col_l_r <- round(col_l_r / max(col_l_r) * 255)
col_l_g <- round(col_l_g / max(col_g_r) * 255)
col_l_b <- round(col_l_b / max(col_b_r) * 255)
col_r_r <- round(col_r_r / max(col_r_r) * 255)
col_r_g <- round(col_r_g / max(col_g_r) * 255)
col_r_b <- round(col_r_b / max(col_b_r) * 255)

# Convert to hexadecimal
col_l_r <- as.hexmode(col_l_r)
col_l_g <- as.hexmode(col_l_g)
col_l_b <- as.hexmode(col_l_b)
col_r_r <- as.hexmode(col_r_r)
col_r_g <- as.hexmode(col_r_g)
col_r_b <- as.hexmode(col_r_b)

# Paste together for colour vector
col_l <- paste0("#", col_l_r, col_l_g, col_l_b)
col_r <- paste0("#", col_r_r, col_r_g, col_r_b)

# Main loop through for each frame
progress <- 0
for(i in 1:n_frames){
    # Make the double-plot
    double_plot(
        spectrum_left = filter(fft_data, channel == "l" & frame == i),
        spectrum_right = filter(fft_data, channel == "r" & frame == i),
        colour_1 = col_l[i],
        colour_2 = col_r[i],
        contrast = contrast[i])
    
    # Save the plot
    ggsave(paste0(output_dir, i, ".png"), width = frame_width, height = frame_height, units = "px")
    
    # Update progress
    if(progress != round(i*100/n_frames)){
        progress <- round(i*100/n_frames)
        print(paste0("Progress: ", progress, "%"))
    }
}

# Put the plots together in video format
av_encode_video(
    paste0(output_dir, 1:n_frames, ".png"),
    "video.mp4",
    framerate = video_fps,
    audio = song_file
)
