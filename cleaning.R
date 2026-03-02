library(readr)
library(dplyr)
library(purrr)
library(tidyr)


# CHES datasets ----

# Load the CHES codes for later combining of datasets
ches_countries <- read_csv("ches-party-codebooks.csv", na = "", locale = locale(encoding = "UTF-8")) |> 
  select(countrycode, countryshort, countryname) |> 
  distinct()

# Loading the CHES scores
ches_trend_1999_2019 <- read_csv("1999-2019_CHES_dataset_means(v3).csv", na = "", locale = locale(encoding = "ASCII")) #|> 
  select(countrycode = country, party_id, party, year)

# Partyfacts dataset ----
  
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

# sequencing to get all a row per each year in the dataset

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

# Saving some columns to combine later with the wide dataset

columns_keep <- partyfacts_years |> 
  select(year, partyfacts_id, country, name_short)
    
# pivoting wider only for the dataset party ids

partyfacts_wider <- partyfacts_years |>
  pivot_wider(id_cols = c(partyfacts_id, year),
              id_expand = TRUE,
              names_from = dataset_key,
              values_from = dataset_party_id)

# removing lines where either ches or manifesto ids are missing

partyfacts_wider_na <- partyfacts_wider |> 
  filter(!is.na(ches) & !is.na(manifesto))

# combining back the columns we want to keep 
# ISSUE: parties have multiple names due to several national languages

partyfacts_wider_na <- partyfacts_wider_na |> 
  left_join(columns_keep, by = c("partyfacts_id", "year"), keep =T, relationship = "many-to-many") 


# converting manifesto id to numeric

partyfacts_belgium <- partyfacts_wider_na |>
  mutate(manifesto = as.double(manifesto))


# Creating a new column that combines party names that are in two languages

partyfacts_belgium_clean <- partyfacts_belgium |>
  group_by(manifesto, year, ches, partyfacts_id) |>
  summarise(
    name_bilingual = paste(unique(name_short), collapse = " / "),
    .groups = "drop"
  )


# Creating a corpus summary ---

# Loading the Belgium manifesto project corpus data to R

corpus_Belgium <- read_csv("data/corpus_Belgium.csv", 
                           col_types = cols(year = col_double()))

# Creating a summary of how many lines per party per year

corpus_Belgium_summary <- corpus_Belgium |> 
  group_by(party, year) |> 
  summarise(count = n(), .groups = "drop")

# Combining datasets ----

# Adding ches codes to the corpus df to eventually merging the ches data

corpus_belgium_bilingual <- corpus_Belgium_summary |>
  rename(manifesto = party) |>  # changing the column name in the corpus to match 
  inner_join(partyfacts_belgium_clean, by = c("manifesto", "year"), keep = F) # keeping only rows with matching keys

# adding ches codes to the corpus and removing unneeded columns

corpus_belgium_ches <- 
  corpus_belgium_bilingual |>
  left_join(partyfacts_belgium_clean, by = c("manifesto", "year"), keep = F) # |> 
  select(-c(country, name_short))
  
# Loading the CHES scores
ches_trend_1999_2019 <- read_csv("1999-2019_CHES_dataset_means(v3).csv", na = "", locale = locale(encoding = "ASCII")) #|> 
select(countrycode = country, party_id, party, year)

# filtering the ches data to only include codes for Belgium

# Checking which ches codes and years are in the corpus
  
belgium_codes <- corpus_belgium_ches |>
  distinct(ches) |>
  pull(ches)

belgium_years <- corpus_belgium_ches |>
  distinct(year) |>
  pull(year)

# Filtering the ches dataset to include only years and parties in the corpus

ches_Belgium <- 
  ches_trend_1999_2019 |> 
  filter(party_id %in% belgium_codes,
         year %in% belgium_years)


# merging the ches scores to the corpus

corpus_belgium_final <- 
  corpus_belgium_ches |> 
  mutate(ches = as.double(ches)) |>
  left_join(ches_Belgium, by = c("ches" = "party_id", "year"))








