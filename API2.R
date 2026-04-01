library(manifestoR)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)

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
  select(party, date)

View(manifesto_ids)

# This results in 1240 potential manifesto documents to retrieve from 27 countries

# Retrieving the corpus ----

# test sample of a random number

request <- manifesto_ids[3, 1:2]

is_valid <- function(result) {
  !is.null(result) || nrow(res) > 0
}



test <- tryCatch(expr = {res <- mp_corpus_df(request,
                     translation = "en")
                  if (is.null(res) || nrow(res) == 0) {
                    
                  message("No data for party: ", request$party, ", date: ", request$date)
                  return(NA)}
message("Successfully retrieved manifesto for party:", request$party, ", date: ", request$date)

return(res)},

                 error = function(e) {
                   message("Error retrieving manifesto for ", request$party, " in ", request$date, ": ", e$message)
                   return(NA)
                 })

View(test)

?tryCatch

# creating a csv file of the test manifesto

write_csv(test, paste0("data/", request$party, "_", request$date, ".csv"))

# creating a function to retrieve all manifestos

retrieve_manifesto <- function(manifesto_id) {
  test <- mp_corpus_df(manifesto_id, translation = "en")
  return(test)
}

# generalising to go through all manifestos

log_df <- data.frame(
  row = 1:1240,
  party = NA,
  date = NA,
  status = NA
)


for (i in 1:30) {
  log_df$party[i] <- manifesto_ids$party[i]
  log_df$date[i] <- manifesto_ids$date[i]
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

# quite many without english translation -> maybe going through all the files first

View(res)