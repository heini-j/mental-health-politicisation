library(manifestoR)
library(dplyr)
library(readr)
library(tidyr)

# Connecting to the API --------------

# Setting the API key
mp_setapikey("manifesto_apikey.txt")

# Loading the manifesto id:s

manifesto_ids <- read_csv("data/manifesto_ids.csv")

# Creating a loop to retrieve the corpus ----

retrieve_manifesto <- function(manifesto_id) {
  result <- mp_corpus_df(manifesto_id, translation = "en")
  return(result)
}

# Creating an empty df to store the processed ids

processed_ids <- data.frame(row = 1:923, party = NA, date = NA)

# Looping through all the manifesto ids

for (i in 1) {
  message("Retrieving row ", i)
  tryCatch(expr = {res <- retrieve_manifesto(manifesto_ids[i, 1:2])
  write_csv(res, paste0("data/", manifesto_ids$party[i], "_", manifesto_ids$date[i], ".csv"))
  message ("Successfully retrieved row ", i)
  }, error = function(e) {
    message("Error retrieving row ", i, ": ", e$message)
  },
  finally = {
    processed_ids$party[i] <- manifesto_ids$party[i]
    processed_ids$date[i] <- manifesto_ids$date[i]
  })
}


