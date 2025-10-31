library(readr)
library(dplyr)
library(ggplot2)

# Reading data to R ------

data_australia <- read_csv("data/corpus_Australia.csv")

data_australia <- data_australia |>
  mutate(party = as.character(party))

# Getting basic counts -----

# How many rows per year?

corpus_count <- data_australia |>
  group_by(year) |>
  summarise(count = n()) |>
  arrange(year)

View(corpus_count)

# How many rows per party?

corpus_party_count <- data_australia |>
  group_by(party) |>
  summarise(count = n()) |>
  arrange(desc(count))

View(corpus_party_count)

# How many rows per party per year?

corpus_party_year_count <- data_australia |>
  group_by(party, year) |>
  summarise(count = n()) |>
  arrange(party, year)

View(corpus_party_year_count)

# Visualizing counts over time -----

# Creating a line plot showing the count of rows per year for each party

ggplot(corpus_party_year_count, aes(x = year, y = count, color = party, group = party)) +
  geom_line() +
  labs(title = "Count of Manifesto Rows per Year",
       x = "Year",
       y = "Count of Rows") +
  theme_minimal() + 
  scale_color_brewer(palette = "Set1")


?geom_line
