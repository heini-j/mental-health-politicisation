library(manifestoR)
library(dplyr)
library(readr)
library(tidyr)


# Connecting to the API --------------

# Setting the API key
mp_setapikey("manifesto_apikey.txt")

# Loading the main dataset -----
main_df <- mp_maindataset() # corpus version 2025-01

# Choosing only countries included in the CHES dataset

ches_countries <- read_csv("ches-party-codebooks.csv") |>
  select(countryname) |>
  distinct() # 52 countries

manifesto_countries <- main_df |>
  select(countryname) |>
  distinct() # 67 countries

# Checking how the two datasets overlap

# Which countries are only in the manifesto dataset?

main_filtered <- manifesto_countries |>
  filter(!countryname %in% ches_countries$countryname) |>
  arrange(countryname) # For easier comparison

View(main_filtered) # 21 countries not in CHES

# Which countries are only in CHES?

ches_filtered <- ches_countries |>
  filter(!countryname %in% manifesto_countries$countryname) |>
  arrange(countryname) # for easier comparison

View(ches_filtered) # 6 countries not in manifesto dataset

# Bosnia-Hetzegovina, Peru, Uruguay in both -> need to change name
# in CHES : PERU to Peru, Urugay to Uruguay, Bosnia & Herzegovina to Bosnia-Herzegovina

ches_countries_corrected <- ches_countries |>
  mutate(countryname = case_when(
    countryname == "PERU" ~ "Peru",
    countryname ==  "Urugay" ~ "Uruguay",
    countryname == "Bosnia & Herzegovina" ~ "Bosnia-Herzegovina",
    TRUE ~ countryname
  ))

# Checking that it worked

main_filtered <- manifesto_countries |>
  filter(!countryname %in% ches_countries_corrected$countryname) |>
  arrange(countryname) # 18 countries discarded, in europe Belarus, DDR, Northern Ireland, Moldova, Russia

ches_filtered <- ches_countries_corrected |>
  filter(!countryname %in% manifesto_countries$countryname) |>
  arrange(countryname) # Kosovo, Paraguay, Venezuela discarded

# Shared n of countries is 49

# Filtering for the right time period

main_filtered_time <- main_df |>
  filter(countryname %in% ches_countries_corrected$countryname) |> 
  filter(date > 199812)  #CHES starts from 1999

View(main_filtered_time) # 2115 rows

# Does this impact the sample of countries?

countries_sample <- main_filtered_time |>
  select(countryname) |>
  distinct() # all 49 countries are still included

# Checking basic descriptives of the potential sample

# checking the number of manifestos per country

preli_stats <- main_filtered_time |>
  group_by(countryname) |>
  summarise(n = n()) |>
  arrange(desc(n)) 

# checking the time coverage of the manifestos per country

preli_coverage <- main_filtered_time |>
  group_by(countryname) |>
  summarise(
    min_year = as.numeric(substr(min(date), 1, 4)),
    max_year = as.numeric(substr(max(date), 1, 4)),
    year_difference = max_year - min_year
  ) |>
  arrange(year_difference) # checking the time period of manifestos per country

# Uruguay has only one measurement, it will be discarded

# Checking the number of manifestos per year

preli_years <- main_filtered_time |>
  mutate(year = as.numeric(substr(date, 1, 4))) |>
  group_by(year) |>
  summarise(n = n()) |>
  arrange(desc(year))

# Recent years have less manifestos 


# Making a list of manifesto id:s to use for later data handling

manifesto_ids <- main_filtered_time |>
  filter(countryname != "Uruguay") |>
  select(party, date, countryname)

View(manifesto_ids)

# 2110 potential manifesto documents

# Retrieving the corpus ----

# function for checking if the requested manifesto has an english translation
is_valid <- function(result) {
  nrow(res) > 0
}

# creating a function to retrieve all manifestos

retrieve_manifesto <- function(manifesto_id) {
  test <- mp_corpus_df(manifesto_id, translation = "en")
  return(test)
}

# testing the functions

res <- retrieve_manifesto(request)

is_valid(res)

# test sample of a random number

request <- manifesto_ids[994, 1:2]

# creating an empty file to log the availability of english translation for each manifesto

log_df <- data.frame(
  row = 1:2110,
  party = NA,
  date = NA,
  countryname = NA,
  status = NA
)

# looping through all the manifestos and retrieving the data to check availability

for (i in 1:nrow(manifesto_ids)) {
  log_df$party[i] <- manifesto_ids$party[i]
  log_df$date[i] <- manifesto_ids$date[i]
  log_df$countryname[i] <- manifesto_ids$countryname[i]
  message("Retrieving row ", i, "out of 2110")
  res <- retrieve_manifesto(manifesto_ids[i, 1:2])
  if (!is_valid(res)) {
    log_df$status[i] <- 0
    message("  -> No data available")
  } else {
    # write_csv(res, paste0("data/", party, "_", date, ".csv"))
    log_df$status[i] <- 1
  }
}

View(log_df)

# saving the log file to avoid having to run the loop again

write_csv(log_df, "data/data_availability.csv")

# checking the availability of the manifestos  ---- 

log_df <- read_csv("data/data_availability.csv")

availability_summary <-  log_df |>
  group_by(countryname) |>
  summarise(
    total = n(),
    available = sum(status == 1),
    unavailable = sum(status == 0),
    availability_rate = available / total
  ) |>
  arrange(desc(availability_rate))

View(availability_summary)

# Israel, Bosnia-Herzegovina, Albania, Croatia, Iceland, Malta, Montenegro, North Macedonia and Serbia have no availability (1 doc for israel)


# filtering out countries with less than 50 % availability

available_countries <- availability_summary |>
  filter(availability_rate >= 0.5) |>
  select(countryname)

# 52 countries left

availability_stats <- log_df |>
  filter(countryname %in% available_countries$countryname) |>
  # group_by(countryname) |>
  summarise(
    total = n(),
    available = sum(status == 1),
    unavailable = sum(status == 0)
  ) 

View(availability_stats)

# This gives 1627 manifestos of which 1331 have an english translation -> 82 % availability

# Checking availability per year

availability_stats_years <- log_df |>
  filter(countryname %in% available_countries$countryname) |>
  mutate(year = as.numeric(substr(date, 1, 4))) |>
  group_by(year) |>
  summarise(
    total = n(),
    available = sum(status == 1),
    unavailable = sum(status == 0),
    availability_rate = available / total)

View(availability_stats_years)

 # AFter 2004 very good availability

# Creating a final list of manifestos to retrieve

manifesto_ids_final <- log_df |>
  filter(countryname %in% available_countries$countryname) |>
  filter(status == 1) |>
  select(party, date, countryname)

# Saving as a csv file

write_csv(manifesto_ids_final, "data/manifesto_ids_final.csv")
