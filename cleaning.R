library(readr)
library(dplyr)
library(purrr)
library(tidyr)

# Loading the manifesto file to R ----

corpus_Belgium <- read_csv("data/corpus_Belgium.csv", 
                      col_types = cols(year = col_double()))

# summarising for how many items per year and party

corpus_Belgium_summary <- corpus_Belgium |> 
  group_by(party, year) |> 
  summarise(count = n(), .groups = "drop")


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
    
# pivoting wider only for the dataset party ids

partyfacts_wider <- partyfacts_years |>
  pivot_wider(id_cols = c(partyfacts_id, year),
              id_expand = TRUE,
              names_from = dataset_key,
              values_from = dataset_party_id)

# combining back the columns we want to keep

partyfacts_wider <- partyfacts_wider |> 
  left_join(columns_keep, by = c("partyfacts_id", "year"), keep = F) 


# FOr the test case of Belgium, selecting only the right country and rows with ches and manifesto ids

partyfacts_belgium <- partyfacts_wider |> 
  filter(country == "BEL",
         !is.na(ches),
         !is.na(manifesto)) |>
  mutate(manifesto = as.double(manifesto))



# combining the two different language party names to the same row

partyfacts_belgium_clean <- partyfacts_belgium |>
  group_by(manifesto, year, ches, partyfacts_id) |>
  summarise(
    name_bilingual = paste(unique(name_short), collapse = " / "),
    .groups = "drop"
  )

# Adding ches ids to the corpus df to eventually merging the ches data

corpus_belgium_bilingual <- corpus_Belgium_summary |>
  rename(manifesto = party) |>
  left_join(partyfacts_belgium_clean, by = c("manifesto", "year"), keep = F)

# adding ches codes to the corpus and removing unneeded columns

corpus_belgium_ches <- 
  corpus_belgium_bilingual |>
  left_join(partyfacts_belgium_clean, by = c("manifesto", "year"), keep = F) # |> 
  select(-c(country, name_short))

?left_join

# Adding CHES scores to the corpus ----

# filtering the ches data to only include codes for Belgium

belgium_codes <- corpus_belgium_ches |>
  distinct(ches) |>
  pull(ches)

belgium_years <- corpus_belgium_ches |>
  distinct(year) |>
  pull(year)


ches_Belgium <- 
  ches_trend_1999_2019 |> 
  filter(party_id %in% belgium_codes,
         year %in% belgium_years)


# merging the ches scores to the corpus

corpus_belgium_final <- 
  corpus_belgium_ches |> 
  mutate(ches = as.double(ches)) |>
  left_join(ches_Belgium, by = c("ches" = "party_id", "year"))

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
  



