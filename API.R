library(manifestoR)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)

# Connecting to the API --------------

# Setting the API key
mp_setapikey("manifesto_apikey.txt")

mp_maindataset()

# reading in the dataset with the the party information ----

main_df <- read_csv("MPDataset_MPDS2025a.csv")

# seeing which countries were eu members during the latest input in the dataset

main_filtered <- main_df |>
  group_by(countryname) |>
  filter(eumember[which.max(date)] == 10) |> # choosing countries that were EU members during the last election in the data set
  ungroup() |>
  filter(date > 199812)  #CHES starts from 1999

View(main_filtered)

# Making a list of manifesto id:s for later use

manifesto_ids <- main_filtered |>
  mutate(manifesto_id = paste0(party, "_", date)) |>
  select(manifesto_id)|>
  pull()
  
# 1240 potential manifesto documents to retrieve from 27 countries


# Defining relevant filters ----

# making a vector containing all the countries in the dataset

party <- strsplit(manifesto_ids[65], "_")[[1]][1]
date <- strsplit(manifesto_ids[65], "_")[[1]][2]

test <- mp_corpus(party == party, 
                  date == date, 
                  as_tibble = TRUE)

?mp_corpus

test_belgium <- mp_corpus(countryname == "Belgium", 
                        translation = "en",
                        as_tibble = TRUE)

View(test_belgium)

#saving as a csv for later use

write_csv(test_belgium, "data/corpus_Belgium.csv")

head(test_belgium$text)

sample_test <- test_belgium |>
  slice_sample(n=30, replace = FALSE)

View(sample_test)


# creating a list of relevant keywords
keywords <- c("mental health", "mental illness", "depression","anxiety","ADHD", "autism", "therapy")

# adding all words starting with psych to the pattern
pattern <- paste0("\\b(", paste(keywords, collapse = "|"), ")\\b|\\bpsych\\w*")

# Creating a function to retrieve data for all the countries ----

# creating an empty list to put all countries without data in

no_data_countries <- c()

# Function to create a CSV for each country ----

create_country_csv <- function(country_name) {
  # Try to retrieve corpus data
  country_corpus <- tryCatch(
    mp_corpus(countryname == country_name, 
              translation = "en",
              as_tibble = TRUE),
    error = function(e) {
      message(paste("Error retrieving data for", country_name, ":", e$message))
      return(NULL)
    })
  
  # If no corpus retrieved or empty, log and continue
  if (is.null(country_corpus) || nrow(country_corpus) == 0) {
    message(paste("No data available for", country_name))
    no_data_countries <<- c(no_data_countries, country_name)
    return(invisible(no_data_countries))
  }
  else {
  # Clean and prepare corpus
  corpus_id <- country_corpus |>
    mutate(rownumber = row_number(), # to be able to find unclear rows later
           year = as.numeric(substr(date, 0, 4))) |>  # we only need year level data
    select(!c(eu_code, cmp_code, manifesto_id, annotations, translation_en)) # removing unneeded columns
  
  # Filter relevant text
  corpus_filtered <- corpus_id |>
    filter(!is.na(text) & grepl(pattern, text, ignore.case = TRUE)) |> # removing empty rows and including those with keywords
    distinct(text, .keep_all = TRUE) } # removing duplicates
  
  # Save to CSV if data left after filtering
  if (nrow(corpus_filtered) > 0) {
  write.csv(corpus_filtered,
            paste0("data/corpus_", country_name, ".csv"),
            row.names = FALSE)
  print(paste("Created CSV for", country_name))
  return(invisible(NULL))}
  
  # in case of no relevant rows of text
  else {
    message(paste("No relevant data found for", country_name))
    no_data_countries <<- c(no_data_countries, country_name)
    return(invisible(no_data_countries))
  }
}

# Creating CSV for each country ----

# Looping through the countries and creating a csv for each

for (country in countries) {
  create_country_csv(country)
}


# Viewing countries with no data

no_data_countries


