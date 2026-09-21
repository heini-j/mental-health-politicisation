library(manifestoR)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(stringr)

mp_cite()

#' This file is for retrieving original language manifesto documents from the Manifesto Project API. 
#' The sample is limited to Nordic countries (Denmark, Finland, Norway, Sweden) and to elections before 1999, which were not covered by earlier search.
#' The code first idetifies the manifesto codes of interest.
#' Then functions are created for retrieving the manifestos and parsing the text to multiple lines for further processing. 
#' Last, the documents are saved along with lists of the manifesto ids.

# Creating a list of manifesto ids --------------

# Loading the newest version of the main dataset -----
main_df <- mp_maindataset() # 01-2026 version

# Filtering the main dataset to the Nordic countries and dates not covered by earlier search

main_filtered <- main_df |>
 filter(countryname %in% c("Denmark", "Finland", "Norway", "Sweden"),
        date < 199901) |>
  group_by(countryname, date) |>
  slice_max(order_by = pervote, n = 3) |> # selecting top 3 parties of each election to limit the sample
  ungroup()

View(main_filtered)

# Checking that the time frame is correct 

min(main_filtered$date) #192011
max(main_filtered$date) #199809


# Making a list of manifesto id:s for later use

manifesto_ids <- main_filtered |>
  select(party, date, countryname)

View(manifesto_ids)

summarise(manifesto_ids,
          denmark = sum(countryname == "Denmark"),
          finland = sum(countryname == "Finland"),
          norway = sum(countryname == "Norway"),
          sweden = sum(countryname == "Sweden"))

# denmark 67, fin 45, nor 42, swe 54

# 208 potential manifesto documents to retrieve from the four countries

# Test for manifesto retrieval, parsing and saving ----

# test with one manifesto document

request <- manifesto_ids[30, ]

request$date
request$party

names(request)

# retriecing the document 
test <- mp_corpus_df(request)

# splitting the text into multiple lines
test_split <- test |>
  separate_longer_delim(text, delim = ".") |>
  mutate(text = str_trim(text))

View(test_split)

# creating a csv file of the test manifesto

write_csv(test, paste0("data/", request$party, "_", request$date, ".csv"))

# creating anecessary functions ----

# Setting the API key
mp_setapikey("manifesto_apikey.txt")

# function to retrieve and parse manifesto

retrieve_manifesto <- function(manifesto_id) {
  test <- mp_corpus_df(manifesto_id)
  test_split <- test |>
    separate_longer_delim(text, delim = ".") |>
    mutate(text = str_trim(text))
  write_csv(test_split, paste0("~/Google Drive/My Drive/Colab Notebooks/Thesis/new_files/", manifesto_id$party, "_", manifesto_id$date, ".csv"))
  return(test_split)
}

# a function to check if the corpus has digitised version of the manifesto

is_valid <- function(res) {
  !is.null(res) && nrow(res) > 0
}

# a dataframe that will log the availability information for each document
log_df <- data.frame(
  row = 1:208,
  status = NA,
  stringsAsFactors = FALSE
)

# Looping through all the manifestos ----

for (i in 1:nrow(manifesto_ids)) {
  message("Retrieving row ", i, " of ", nrow(manifesto_ids))
  res <- tryCatch(retrieve_manifesto(manifesto_ids[i, 1:2]),
                      error = function(e) {
                        message(paste("Error retrieving row ", i, ": ", e$message))
                        return(NULL)
                      })
  
  if (!is_valid(res)) {
    log_df$status[i] <- "no_data"
    message("  -> No data available")
  } else {
    log_df$status[i] <- "success"
  }
}

# Checking the logs -----

View(log_df)

log_df <- left_join(manifesto_ids, log_df, by = c("row" = "row"))

log_df |>
  group_by(countryname) |>
  summarise(
    total = n(),
    success = sum(status == "success"),
    no_data = sum(status == "no_data")
  )


# 151 manifestos successfully retrieved, 57 manifestos with no data available

# limiting the list of ids to those that had a document

success_ids <- manifesto_ids[log_df$status == "success", ]

write_csv(success_ids, "~/Google Drive/My Drive/Colab Notebooks/Thesis/additional_ids.csv")

# examining the available documents in relation to the not available ones

min(success_ids$date) # 194510
max(success_ids$date) # 199809


summarise(success_ids,
          denmark = sum(countryname == "Denmark"),
          finland = sum(countryname == "Finland"),
          norway = sum(countryname == "Norway"),
          sweden = sum(countryname == "Sweden"))

# 45 manifestos from Denmark, 27 from Finland, 40 from Norway, and 39 from Sweden 
# -> almost all Norwegian manifestos were included

# saving the lists by country for further analysis

ids_finland <- success_ids |>
  filter(countryname == "Finland")

write_csv(ids_finland, "~/Google Drive/My Drive/Colab Notebooks/Thesis/ids_finland.csv")

ids_denmark <- success_ids |>
  filter(countryname == "Denmark")

write_csv(ids_denmark, "~/Google Drive/My Drive/Colab Notebooks/Thesis/ids_denmark.csv")

ids_norway <- success_ids |>
  filter(countryname == "Norway")

write_csv(ids_norway, "~/Google Drive/My Drive/Colab Notebooks/Thesis/ids_norway.csv")

ids_sweden <- success_ids |>
  filter(countryname == "Sweden")

write_csv(ids_sweden, "~/Google Drive/My Drive/Colab Notebooks/Thesis/ids_sweden.csv")

mp_cite()
