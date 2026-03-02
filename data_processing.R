library(readr)
library(dplyr)
library(ggplot2)

# This script is for descriptive analysis of the corpus data 

# Reading data to R ------

data_belgium <- read_csv("data/belgium_with_ches.csv")

# Basic descriptives -----

# Adding together all values in the "count" column to get the total count for Belgium

total_count_belgium <- sum(data_belgium$count) # 502 in total

# How many rows per year?

corpus_count <- data_belgium |>
  group_by(year) |>
  summarise(count_yearly = sum(count)) |>
  arrange(year)

View(corpus_count)

unique(data_belgium$year)

# How many rows per party?

corpus_party_count <- data_belgium |>
  group_by(name_bilingual) |>
  summarise(count_party = sum(count)) 

View(corpus_party_count)

# How many rows per party per year?

corpus_party_year_count <- data_belgium |>
  group_by(name_bilingual, year) |>
  summarise(count_party_year = sum(count))

View(corpus_party_year_count)

