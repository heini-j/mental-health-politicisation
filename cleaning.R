library(readr)
library(dplyr)
library(purrr)
library(tidyr)

# Loading the manifesto file to R ----

corpus_Belgium <- read_csv("data/corpus_Belgium.csv", 
                      col_types = cols(year = col_double()))



# Load the data + specify the variables to be used
ches_countries <- read_csv("ches-party-codebooks.csv", na = "", locale = locale(encoding = "UTF-8")) |> 
  select(countrycode, countryshort, countryname) |> 
  distinct()



ches_trend_1999_2019 <- read_csv("1999-2019_CHES_dataset_means(v3).csv", na = "", locale = locale(encoding = "ASCII")) #|> 
  select(countrycode = country, party_id, party, year)

# loading the data to R 
  
partyfacts <- 
  read_csv("partyfacts-external-parties.csv", locale = locale(encoding = "UTF-8")) |> 
  filter(dataset_key %in% c("ches", "manifesto"),
         country == "BEL") |>
  select(country, 
         partyfacts_id, 
         dataset_key, 
         dataset_party_id, 
         name_short,
         year_first,
         year_last)

problems(partyfacts)

# sequencing to get all years for each party

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

# pivoting wider to get dataset party ids in columns

columns_keep <- partyfacts_years |> 
  select(year, partyfacts_id, country, name_short)
    
# 

partyfacts_wider <- partyfacts_years |>
  pivot_wider(id_cols = c(partyfacts_id, year),
              id_expand = TRUE,
              names_from = dataset_key,
              values_from = dataset_party_id)

partyfacts_wider <- partyfacts_wider |> 
  left_join(columns_keep, by = c("partyfacts_id", "year")) 




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
  



