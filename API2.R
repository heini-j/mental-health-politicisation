library(manifestoR)
library(dplyr)
library(readr)
library(tidyr)


#' This code is for connecting to the manifesto corpus through the API and checking
#' the availability of manifestos based on selection of countries (with English translation)

# Connecting to the API --------------

# Setting the API key
mp_setapikey("manifesto_apikey.txt")

# Loading the main dataset -----
main_df <- mp_maindataset() # corpus version 2026-01

# Creating necessary functions ----

# function for checking if the requested manifesto exists in the corpus (i.e. is not an empty df)
is_valid <- function(result) {
  nrow(res) > 0
}

# a function for retrieving a manifesto from the corpus based on the manifesto id (party code and date)

retrieve_manifesto <- function(manifesto_id) {
  test <- mp_corpus_df(manifesto_id, translation = "en")
  return(test)
}

# test sample of a random number

request <- manifesto_ids[994, 1:2]

# testing the functions

res <- retrieve_manifesto(request)

is_valid(res)


# creating an empty file to log the availability of the manifestos

log_df <- data.frame(
  row = 1:nrow(manifesto_ids),
  party = NA,
  date = NA,
  countryname = NA,
  status = NA
)

# Retrieving the manifestos ----

# looping through all the manifestos and retrieving the data to check availability

for (i in 1:nrow(manifesto_ids)) {
  
  log_df$party[i] <- manifesto_ids$party[i]
  log_df$date[i] <- manifesto_ids$date[i]
  log_df$countryname[i] <- manifesto_ids$countryname[i]
  
  message("Retrieving row ", i, "out of ", nrow(manifesto_ids))
  
  res <- retrieve_manifesto(manifesto_ids[i, 1:2])
  
  if (!is_valid(res)) {
    log_df$status[i] <- 0
    message("  -> No data available")
  } else {
    write_csv(res, paste0("data/", party, "_", date, ".csv"))
    log_df$status[i] <- 1
  }
}

View(log_df)

# saving the log file to avoid having to run the loop again

write_csv(log_df, "data/data_availability.csv")

# checking the availability of the manifestos  ---- 

# log_df <- read_csv("data/data_availability.csv")

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

# Creating a final list of manifestos to retrieve

manifesto_ids_final <- log_df |>
  filter(countryname %in% available_countries$countryname) |>
  filter(status == 1) |>
  select(party, date, countryname)

# Saving as a csv file

write_csv(manifesto_ids_final, "data/manifesto_ids.csv")
