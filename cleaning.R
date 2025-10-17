library(readr)
library(dplyr)

# Loading the manifesto file to R ----

data_australia <- read_csv("data/corpus_Australia.csv")

View(data_australia)

# Cleaning the data ----

# The data contains a lot of duplicates - let's find them

repeated <- unique(data_australia$text)

length(repeated)

# 499 unique texts - many are multiple times in the dataset

# Let's remove the duplicates

df_clean <- data_australia |>
  distinct(text, .keep_all = TRUE)

View(df_clean)

