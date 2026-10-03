library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

#' This code is for combining the summary of encoded manifesto documents 
#' with the some variables from the main Manifesto dataset and the CHES dataset.
#' The combining uses a third dataset, "partyfacts", which hhas the party ids for both 
#' the Manifesto project and the CHES dataset.

# reading the data to R ------

# Summary data for the LLM scores of the manifesto documents

summary <- read_csv("data/summary.csv", col_types = cols(party = col_character()))

View(summary)

# Adding some key variables to the summary

summary <- summary |>
  mutate(
    year =  as.integer(substr(date, 1, 4)),
    month = substr(date, 5,6),
    ratio = rows_classified_1/total_rows
  ) |>
  rename("manifesto" = "party")

names(summary)

# Checking the number of parties
parties <- summary |>
  select("manifesto") |>
  distinct() |>
  pull(manifesto)

# The manifesto project main dataset to get the party families and names

manifesto <- read_csv("data/MPDataset_MPDS2026a.csv",
                      col_types = cols(edate = col_character(), 
                                       parfam = col_character())) |>
  select("date",
         "manifesto" = "party", 
         "partyname", 
         "MP_parfam" = "parfam", 
         "countryname",
         "MP_pervote" = "pervote",
         "MP_totseats" = "totseats",
         "MP_human_rights" = "per201",
         "MP_keynesian"= "per409",
         "MP_equality" = "per503",
         "MP_welfare_exp" = "per504",
         "MP_welfare_lim" = "per505",
         "MP_labourgroups" = "per701",
         "MP_minorities" = "per705") |> # We don't need all the columns
  filter(manifesto %in% parties) |>
  mutate(manifesto = as.character(manifesto),
         )


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

# Loading the CHES scores for finland, denmark and sweden (Norway is not in this file)
ches <- read_csv("data/1999-2024_CHES_dataset_meansV2.csv") |>
  filter(country %in% c(2, 14, 16), # Sweden, Finland, Denmark
         year %in% c(2006, 2002, 1999)) |>
           select("country",
                  "year",
                  "ches" = "party_id",
                  "lrgen",
                  "lrecon",
                  "galtan",
                  "spendvtax",
                  "redistribution") |>
  mutate(year = replace_when(year, year == 2006 & country == 2 ~ 2007),
         year = replace_when(year, year == 2006 & country == 14 ~ 2007),
         year = replace_when(year, year == 2006 & country == 16 ~ 2006),
         year = replace_when(year, year == 2002 & country == 2 ~ 2001),
         year = replace_when(year, year == 2002 & country == 14 ~ 2003),
         year = replace_when(year, year == 2002 & country == 16 ~ 2002),
         year = replace_when(year, year == 1999 & country == 14 ~ 1999)
         ) |>
           select(-country)

View(ches)

# Adding Norway data separately from the individual rounds of ches
# Norwegian elections were 2005, 2009, 2013, 2017 

# commenting out the items that were not asked in the 2019 survey

ches_2024 <- read_csv("data/CHES_2024_final_v2.csv") |>
  filter(country == 16) |>
  select("ches" = "party_id",
         "lrgen",
         "lrecon",
         "galtan",
         "spendvtax",
         "redistribution") |>
  mutate(year = 2022)


View(ches_2024)

ches_2019 <- read_csv("data/CHES2019V3.csv") |>
  filter(country %in% c(2, 14, 16, 35)) |>
  select("country",
  "ches" = "party_id",
         "lrgen",
         "lrecon",
         "galtan",
         "spendvtax",
         "redistribution") |>
  mutate(
    year = case_when(country == 2 ~ 2019,
                     country == 14 ~ 2019,
                     country == 16 ~ 2018, 
                     country == 35 ~ 2017)
  ) |>
  select(-country)

View(ches_2019)


ches_2014 <- read_csv("data/2014_CHES_dataset_means.csv") |>
  filter(country %in% c(2, 14, 16, 35)) |>
  select("ches" = "party_id",
         "country",
         "lrgen",
         "lrecon",
         "galtan",
         "spendvtax",
         "redistribution") |>
  mutate(
    year = case_when(country  == 2 ~ 2015,
                     country == 14 ~ 2015,
                     country == 16 ~ 2014,
                     country == 35 ~ 2013)
  ) |>
  select(-country)

View(ches_2014)

ches_2010 <- read_csv("data/2010_CHES_dataset_means.csv") |>
  filter(country %in% c(2, 14, 16, 35))  |>
  select("ches" = "party_id",
         "country",
         "lrgen",
         "lrecon",
         "galtan",
         "spendvtax",
         "redistribution") |>
  mutate(
    year = case_when(country == 2 ~ 2011,
                     country == 14 ~ 2011,
                     country == 16 ~ 2010,
                     country == 35 ~ 2009) 
  ) |>
  select(-country)

 
View(ches_2010) 

ches_complete <- bind_rows(ches, ches_2019, ches_2014, ches_2010) |>
  mutate(ches = as.character(ches))


View(ches_complete)

# Preparing datasets for combining ---- 

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
  filter_out(is.na(manifesto) | is.na(ches)) 


# Combining the datasets ------

# Adding partyfacts ids to the summary

summary_w_ids <- left_join(summary, partyfacts_final, by = c("manifesto", "year"))

summary_w_manifesto <- left_join(summary_w_ids, manifesto, by = c("manifesto", "date"))


# for green party there are two values for ches for the same year, because the ches was conducted in 1999 and in 2002

complete <- left_join(summary_w_manifesto, ches_complete, by = c("ches", "year"),
                    relationship = "one-to-one")


View(complete)

# Removing the columns that are not needed for analysis

complete <- complete |>
  select(-c(partyfacts_id, ches)) |>
  rename("party_id" = "manifesto")

# Saving the final dataset for later use -----

write_csv(complete, "data/summary_df.csv")





