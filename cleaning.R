library(readr)
library(dplyr)
library(fuzzyjoin)

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
  read_csv("partyfacts-external-parties.csv", na = "", locale = locale(encoding = "UTF-8")) |> 
  filter(dataset_key %in% c("manifesto")) |> 
  #select(dataset_party_id, partyfacts_id) |> 
  mutate(dataset_party_id = as.numeric(dataset_party_id)) |> 
  distinct()

ches_europe <- 
  bind_rows(
    ches_trend_1999_2019 |> left_join(ches_countries, by = c("countrycode")) |> select(countryshort, year, party_id, party))

ches_all <- 
  bind_rows(
    ches_europe |> mutate(party_id = as.numeric(party_id)))

ches <- 
  ches_all |> 
  mutate(
    year_first = min(year, na.rm = TRUE),
    year_last = max(year, na.rm = TRUE),
    .by = c(party_id)
  ) |> 
  select(-year) |> 
  distinct() |> 
  slice(1L, .by = c("party_id"))

ches$party_id |> duplicated() |> any()

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
  distinct(text, .keep_all = TRUE) |>
  rename("dataset_party_id" = "party")

View(df_clean)

df_joined <- fuzzy_left_join(
  df_clean, partyfacts,
  by = c(
    "party_id" = "dataset_party_id",
    "year_first" ="year",
    "year_last" ="year"
  ),
  match_fun = list(`==`, `>=`, `<=`)
)

?fuzzy_inner_join
