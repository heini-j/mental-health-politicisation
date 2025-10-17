library(readr)
library(dplyr)

# Loading the manifesto file to R ----

data_australia <- read_csv("data/corpus_Australia.csv")

View(data_australia)

# Load the data + specify the variables to be used
ches_countries <- read_csv("ches-party-codebooks.csv", na = "", locale = locale(encoding = "UTF-8")) |> 
  select(countrycode, countryshort, countryname) |> 
  distinct()

ches_trend_1999_2019 <- read_csv("1999-2019_CHES_dataset_means(v3).csv", na = "", locale = locale(encoding = "ASCII")) |> 
  select(countrycode = country, party_id, party, year)

partyfacts <- 
  read_csv("partyfacts-core-parties.csv", na = "", locale = locale(encoding = "UTF-8")) |> 
  filter(dataset_key == "ches") |> 
  select(dataset_party_id, partyfacts_id) |> 
  mutate(dataset_party_id = as.numeric(dataset_party_id)) |> 
  distinct()

ches_partyfacts <- 
  ches |> 
  left_join(partyfacts, by = c("party_id" = "dataset_party_id"))

# Cleaning the data ----

# The data contains a lot of duplicates - let's find them

repeated <- unique(data_australia$text)

length(repeated)

# 499 unique texts - many are multiple times in the dataset

# Let's remove the duplicates keeping only first occurence of each unique value

df_clean <- data_australia |>
  distinct(text, .keep_all = TRUE)

View(df_clean)

