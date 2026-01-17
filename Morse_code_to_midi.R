# Script to read in plain text, then export as midi
# Sam Siljee
# 16 January 2026

# Note the use of a python function to convert from the csv to midi:
# https://github.com/timwedde/py_midicsv
# pip install py_midicsv

# Libraries
library(dplyr) # Data frame and piping etc
library(stringr) # String manipulation

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
note <- 69 #A4 note, 440 Hz
velocity <- 100

# Read in morse code key and intersperse special characters
morse_key <- read.csv(morse_code_file) %>%
    mutate(code = sapply(strsplit(code, ""), paste, collapse="*")) %>%
    bind_rows(
        data.frame(
            char = c("|", "_"),
            code = c("|", "_")
        )
    )

# Create a named lookup vector
lookup <- setNames(morse_key$code, morse_key$char)

# Read in and convert text
input_text <- sapply(strsplit(toupper(trimws(readLines(text_file), "b")), ""), paste, collapse="|") %>%
    str_replace_all("\\| \\|", "_") %>%
    strsplit("") %>%
    unlist() %>%
    {lookup[.]} %>% # Index the lookup vector
    paste(collapse = "")

# Split into a vector
input_text <- as.vector(str_split_fixed(input_text, pattern = "", n = nchar(input_text)))

# Replace characters with timings

# Convert to table

# Set default note and velocities
midi_table$V5 <- note
midi_table$V6 <- velocity

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
