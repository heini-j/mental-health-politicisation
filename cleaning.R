library(readr)
library(dplyr)
library(purrr)
library(tidyr)

# Loading the manifesto file to R ----

data_australia <- read_csv("data/corpus_Australia.csv")

View(data_australia)

# Load the data + specify the variables to be used
ches_countries <- read_csv("ches-party-codebooks.csv", na = "", locale = locale(encoding = "UTF-8")) |> 
  select(countrycode, countryshort, countryname) |> 
  distinct()

ches_trend_1999_2019 <- read_csv("1999-2019_CHES_dataset_means(v3).csv", na = "", locale = locale(encoding = "ASCII")) #|> 
  select(countrycode = country, party_id, party, year)

# loading the data to R 
  
partyfacts <- 
  read_csv("partyfacts-external-parties.csv", locale = locale(encoding = "UTF-8")) #

unique(partyfacts$dataset_key)

|> 
  filter(dataset_key %in% c("ches", "manifesto"),
         country == "AUS")

# creating a dataframe that shows party name for each year

partyfacts_years <- partyfacts |> 
  group_by(rn = row_number()) |>
  mutate(year = list(year_first:year_last)) |>
  unnest(cols = c(year)) |>
  ungroup() |>
  select(country, 
         partyfacts_id, 
         dataset_key, 
         dataset_party_id, 
         name_short,
         year)
    
  pivot_wider(id_cols = partyfacts_id,
              id_expand = TRUE,
              names_from = dataset_key,
              values_from = dataset_party_id) #|>
  select(dataset_party_id, partyfacts_id) |> 
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

ches_partyfacts <- ches_partyfacts |> 
  mutate(year = map2(as.numeric(year_first), as.numeric(year_last), seq = 1, by = 1)) |>
           unnest(year)
  



