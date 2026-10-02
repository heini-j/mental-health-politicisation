library(manifestoR)
library(dplyr)
library(readr)
library(tidyr)
library(tibble)

#' This code is for checking retrieving all the manifesto documents that have been previously
#' logged as "available". The function retrieve_manifesto is used to call the API and request
#' a manifesto based on its manifesto id, comprised of party code and a date of the election in MM YYYY
#' All quasi-sentences from the manifestos are stored in a dataframe called texts_for_sampling,
#' which is then used to sample for the LLM tests.  


# Connecting to the API --------------

# Setting the API key
mp_setapikey("manifesto_apikey.txt")

# Loading the manifesto id:s

manifesto_ids <- read_csv("data/manifesto_ids.csv")

# Creating a function to retrieve a manifesto from the corpus ----

retrieve_manifesto <- function(manifesto_id) {
  result <- mp_corpus_df(manifesto_id, translation = "en")
  return(result)
}

# Creating necessary dfs -----

# Creating an empty df to log which ids have been processed

processed_ids <- data.frame(row = 1:nrow(manifesto_ids), party = NA, date = NA)

# creating a dataframe to store all the texts from the manifestos for future use

texts_for_sampling <- data.frame(text=character())

# Looping through all the manifesto ids ----

for (i in 1:nrow(manifesto_ids)) {
  message("Retrieving row ", i)
  tryCatch(expr = {res <- retrieve_manifesto(manifesto_ids[i, 1:2])
  #write_csv(res, paste0("data/", manifesto_ids$party[i], "_", manifesto_ids$date[i], ".csv"))
  new_rows <- data.frame(text = res$text)
  texts_for_sampling <- bind_rows(texts_for_sampling, new_rows)
  message ("Successfully retrieved row ", i)
  }, error = function(e) {
    message("Error retrieving row ", i, ": ", e$message)
    break
  },
  finally = {
    processed_ids$party[i] <- manifesto_ids$party[i]
    processed_ids$date[i] <- manifesto_ids$date[i]
  })
}

# checking the log

View(processed_ids)


# Saving all the texts from the manifestos to a csv file for later use in sampling

write_csv(texts_for_sampling, paste0("data/all_texts.csv"))


