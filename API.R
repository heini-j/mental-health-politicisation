library(manifestoR)
library(dplyr)
library(stringr)
library(readr)

# Connecting to the API --------------

# Setting the API key
mp_setapikey("manifesto_apikey.txt")

mp_maindataset()

# getting the party information ----

MPDataset_MPDS2025a <- read_csv("MPDataset_MPDS2025a.csv")
View(MPDataset_MPDS2025a)

# making a vector containing all the countries in the dataset

countries <- unique(MPDataset_MPDS2025a$countryname)

# removing Iceland from the list since there is no data

# countries <- countries[!countries %in% c("Iceland", "Northern Ireland", "Malta", "Sri Lanka", "Albania", 
                                         # "Armenia", "Azerbaijan", "Belarus", "Bosnia-Herzegovina", "Bulgaria",
                                         # "Croatia", "Georgia", "German Democratic Republic", "North Macedonia",
                                         # "Montenegro", "Serbia", "South Korea")]


# creating a list of relevant keywords
keywords <- c("mental health", "mental illness", "depression","anxiety","ADHD", "autism", "therapy")

pattern <- paste0("\\b(", paste(keywords, collapse = "|"), ")\\b|\\bpsych\\w*")

# Function to create a CSV for each country ----

create_country_csv <- function(country_name) {
  country_corpus <- tryCatch(
    mp_corpus(countryname == country_name, 
              translation = "en",
              as_tibble = TRUE),
    error = function(e) {
      message(paste("Error retrieving data for", country_name, ":", e$message))
      return(NULL)
    })
  if (is.null(country_corpus) || nrow(country_corpus) == 0) {
    message(paste("No data available for", country_name))
    return(NULL)
  }
  
  
  corpus_id <- country_corpus |>
    mutate(rownumber = row_number(),
           year = as.numeric(substr(date, 0, 4))) |>
    select(!c(eu_code, cmp_code, manifesto_id, annotations, translation_en))
  
  corpus_filtered <- corpus_id |>
    filter(grepl(pattern, text, ignore.case = TRUE))
  
  if (nrow(corpus_filtered) > 0) {
    write.csv(corpus_filtered, 
              paste0("data/corpus_", 
                     country_name, ".csv"), 
              row.names = FALSE) 
    print(paste("Created CSV for", country))}
  else {
    print(paste("Not enough data for", country_name))}
}


# Looping through the countries and creating a csv for each

for (country in countries[1:6]) {
  create_country_csv(country)
}

