library(readr)
library(ggplot2)
library(dplyr)
library(purrr)

# reading the data to R ------

# Summary data for the LLM scores of the manifesto documents

summary <- read_csv("data/summary_nordic.csv")

View(summary)

# The manifesto project main dataset to get the party families and names

manifesto <- read_csv("data/MPDataset_MPDS2026a.csv",
                      col_types = cols(edate = col_character(), 
                                       parfam = col_character())) |>
  select(c("date", "party", "partyname", "parfam")) |> # We don't need all the columns
  filter(party %in% parties) |>
  mutate(party = as.character(party))


View(manifesto)

# The partyfacts dataset to join the ches score with the summary

partyfacts <- 
  read_csv("data/partyfacts-external-parties.csv", locale = locale(encoding = "UTF-8"),
           col_types = cols(partyfacts_id = col_character())) |> 
  filter(dataset_key %in% c("ches", "manifesto"), # we are only interested in these party ids
         country %in% c("SWE", "FIN", "NOR", "DNK")) |> # choosing only the nordic countries
           select(partyfacts_id,
                  dataset_key,
                  dataset_party_id,
                  year_first,
                  year_last,
                  name_short,
                  country) 

# Loading the CHES scores and renaming to match with the corpus df
ches <- read_csv("data/1999-2024_CHES_dataset_meansV2.csv") |> 
  rename(ches = party_id) |>
  mutate(ches = as.character(ches))
  

# Preparing datasets for combining ---- 

# Adding some key variables to the summary

summary <- summary |>
  mutate(
    party = as.character(substr(manifesto_id, 1,5)),
    date = substr(manifesto_id, 7, 12),
    year =  as.integer(substr(manifesto_id, 7, 10)),
    month = substr(manifesto_id, 11,12),
    ratio = rows_classified_1/total_rows*100
  )

# Checking the number of parties
parties <- summary |>
  select("party") |>
  distinct() |>
  pull(party)

# Some parties are listed multiple times due to names in different languages -> combining to one colummn

bilingual_names <- partyfacts|> 
  group_by(partyfacts_id) |>
  summarise(
    name_bilingual = paste(unique(name_short), collapse = " / ")
  )

# Adding to the partyfacts df
bilingual_names <- left_join(bilingual_names, partyfacts, by = "partyfacts_id")

# Partyfacts only has start and end year for each party
# For combining we need all years when the party was active, so we sequence over all years between start and end

partyfacts_years <- bilingual_names |> 
  group_by(rn = row_number()) |>
  mutate(year = list(year_first:year_last)) |>
  unnest(cols = c(year)) |>
  ungroup() |>
  select(partyfacts_id,
         dataset_key,
         dataset_party_id,
         year,
         name_bilingual)


# Saving some columns to combine later with the wide dataset

columns_keep <- partyfacts_years |> 
  select(partyfacts_id, name_bilingual)

# pivoting wider to have the ids for manifesto project and ches as their own columns

partyfacts_wider <- partyfacts_years |>
  pivot_wider(id_cols = c(partyfacts_id, year),
              id_expand = F,
              names_from = dataset_key,
              values_from = dataset_party_id) |>
  filter(year > 1998)

View(partyfacts_wider)

# Some id:s have NAs. Imputing from other rows that match with the two other party id:s

partyfacts_wider <- partyfacts_wider |>
  group_by(partyfacts_id) |>
  mutate(
    manifesto = first(na.omit(manifesto)), # imputing with the first value of manifesto id with the same partyfacts id
    ches = first(na.omit(ches)) # same with the ches party id
  ) |>
  ungroup()


# removing lines where either ches or manifesto is NA - those cannot be used for combining

partyfacts_final <- partyfacts_wider |>
  filter_out(is.na(manifesto) | is.na(ches)) |>
  rename("party" = manifesto) # renaming to match with the summary file


# Combining the datasets ------

# Adding partyfacts ids to the summary

summary_df <- left_join(summary, partyfacts_final, by = c("party", "year"))

# Adding the ches scores to the summary

summary_complete <- left_join(summary_df, ches, by = c("ches", "year")) |>
  rename("party"=party.x) |>
  mutate(date = substr(manifesto_id, 7, 12))

# Adding party families from the manifesto project to the summary
summary_complete$date <- as.double(summary_complete$date)

summary_complete_final <- left_join(summary_complete, manifesto, by = c("party", "date"))

# Checking everything looks good
View(summary_complete_final)

# Saving the combined summary ----

write_csv(summary_complete_final, "data/summary_df.csv")






