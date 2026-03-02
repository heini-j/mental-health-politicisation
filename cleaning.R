library(readr)
library(dplyr)
library(purrr)
library(tidyr)

# This script is for combining data from the manifesto project with data from the Chapel Hill Expert Survey (CHES) using Belgium as a test country. 
# The combination utilises the party ids from the partyfacts dataset to match the two datasets

# Partyfacts dataset ----
  
# loading the data to R 
  
partyfacts <- 
  read_csv("partyfacts-external-parties.csv", locale = locale(encoding = "UTF-8")) |> 
  filter(dataset_key %in% c("ches", "manifesto"), # we're only interested in these two datasets
         country == "BEL") |> # filtering for Belgium only for this test run
  select(country,  
         partyfacts_id, 
         dataset_key, 
         dataset_party_id, 
         name_short,
         year_first,
         year_last)

# sequencing to go from first and last year to s year-level data

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
  filter(!is.na(ches) & !is.na(manifesto)) |>
  mutate(manifesto = as.double(manifesto)) # converting manifesto id to double


# ISSUE: some parties have more names due to several national languages
# -> Creating a new column that combines party names that are in two languages

partyfacts_belgium_clean <- partyfacts_belgium |>
  group_by(manifesto, year, ches, partyfacts_id) |>
  summarise(
    name_bilingual = paste(unique(name_short), collapse = " / "),
    .groups = "drop"
  )


# The corpus data ----

# Loading the manifesto project corpus with relevant lines to R

corpus_Belgium <- read_csv("data/corpus_Belgium.csv", 
                           col_types = cols(year = col_double()))

# We're only interested in number of lines per year and per party, thus summarising the corpus

corpus_Belgium_summary <- corpus_Belgium |> 
  group_by(party, year) |> 
  summarise(count = n(), .groups = "drop")

# Combining datasets ----

# Loading the CHES scores and renaming to match with the corpus df
ches_trend_1999_2019 <- read_csv("1999-2019_CHES_dataset_means(v3).csv", na = "", locale = locale(encoding = "ASCII")) |> 
  rename(ches = party_id) |>
  mutate(ches = as.character(ches)) # converting to double to match with the corpus df)

# Adding ches codes to the corpus df to merge with the ches data

corpus_belgium_bilingual <- corpus_Belgium_summary |>
  rename(manifesto = party) |>  # changing the column name in the corpus to match 
  inner_join(partyfacts_belgium_clean, by = c("manifesto", "year"), keep = F) # keeping only rows with matching keys

# adding all columns from ches dataset to the corpus that have a matching year and party code (ches) in the corpus

corpus_with_ches <- corpus_belgium_bilingual |> 
  inner_join(ches_trend_1999_2019, by = c("ches", "year"), keep = F) # keeping only rows with matching keys

# Saving the combined dataset -----

write_csv(corpus_with_ches, "data/belgium_with_ches.csv")







