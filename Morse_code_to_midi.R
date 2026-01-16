# Script to read in plain text, then export as midi
# Sam Siljee
# 16 January 2026

# Note the use of a python function to convert from the csv to midi:
# https://github.com/timwedde/py_midicsv
# pip install py_midicsv

# Libraries
library(dplyr)

# Input
text_file <- "text.txt"
morse_code_file <- "morse_code.csv"

# Variables
dot_len <- 1
dash_len <- 3
intra_char_len <- 1
inter_char_len <- 3
inter_word_len <- 7
base_len <- 16

# Read in the data
morse_key <- read.csv(morse_code_file)

# Convert to Morse code

# Convert to table

# Add table header
midi_csv <-
    bind_rows(
        data.frame(
            V1 = c(0,1,1),
            V2 = c(0,0,0),
            V3 = c("Header", "Start_track", "Tempo"),
            V4 = as.integer(c(0, NA, 500000)),
            V5 = c(1, NA, NA),
            V6 = c(128, NA, NA)
        ),
        midi_table
    )

# Write table
write.table(midi_csv, file = "midi_csv.csv", row.names = FALSE, quote = FALSE, col.names = FALSE, sep = ",")

# Convert .csv to MIDI using python package
system("csvmidipy midi_csv.csv output_midi.mid")
