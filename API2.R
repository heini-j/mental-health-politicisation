library(manifestoR)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)

# Connecting to the API --------------

# Setting the API key
mp_setapikey("manifesto_apikey.txt")

# Loading the main dataset -----
main_df <- mp_maindataset()

# Filtering to pick only relevant countries

main_filtered <- main_df |>
  group_by(countryname) |>
  filter(eumember[which.max(date)] == 10) |> # choosing countries that were EU members during the last election in the data set
  ungroup() |>
  filter(date > 199812)  #CHES starts from 1999

View(main_filtered) 

# Making a list of manifesto id:s to use for later data handling

manifesto_ids <- main_filtered |>
  select(party, date, countryname)

View(manifesto_ids)

# This results in 1240 potential manifesto documents to retrieve from 27 countries

# Retrieving the corpus ----

# test sample of a random number

request <- manifesto_ids[3, 1:2]

is_valid <- function(result) {
  !is.null(result) || nrow(res) > 0
}


# creating a function to retrieve all manifestos

retrieve_manifesto <- function(manifesto_id) {
  test <- mp_corpus_df(manifesto_id, translation = "en")
  return(test)
}

# creating an empty file to log the availability of english translation for each manifesto

log_df <- data.frame(
  row = 1:1240,
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
  message("Retrieving row ", i)
  res <- retrieve_manifesto(manifesto_ids[i, 1:2])
  if (!is_valid(res)) {
    log_df$status[i] <- 0
    # message("  -> No data available")
  } else {
    # write_csv(res, paste0("data/", party, "_", date, ".csv"))
    log_df$status[i] <- 1
  }
}

View(log_df)

# saving the log file to avoid having to run the loop again

write_csv(log_df, "data/data_availability.csv")

