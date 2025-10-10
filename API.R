library(manifestoR)
library(dplyr)
library(stringr)
library(readr)

# Setting the API key
mp_setapikey("manifesto_apikey.txt")

# Connecting to the API --------------

mp_maindataset()

full_corpus <- mp_corpus_df_bilingual(apikey="manifesto_apikey.txt")

my_corpus_df <- mp_corpus(countryname == "Iceland", translation = "en",
                          as_tibble = TRUE)

# getting the party information 

MPDataset_MPDS2025a <- read_csv("MPDataset_MPDS2025a.csv")
View(MPDataset_MPDS2025a)

# making a vector containing all the countries in the dataset

countries <- unique(MPDataset_MPDS2025a$countryname)

# removing Iceland from the list since there is no data

countries <- countries[!countries %in% c("Iceland", "Northern Ireland", "Malta", "Sri Lanka", "Albania", 
                                         "Armenia", "Azerbaijan", "Belarus", "Bosnia-Herzegovina", "Bulgaria",
                                         "Croatia", "Georgia", "German Democratic Republic", "North Macedonia",
                                         "Montenegro", "Serbia", "South Korea")]


# cleaning by adding row numbers and selecting only relevant columns

corpus_id <- my_corpus_df |>
  mutate(rownumber = row_number(),
         year = as.numeric(substr(date, 0, 4))) |>
           select(!c(eu_code, cmp_code, manifesto_id, annotations, translation_en))


View(corpus_id)

# creating a list of relevant keywords
keywords <- c("mental health", "mental illness", "depression","anxiety","ADHD", "autism", "therapy")

pattern <- paste0("\\b(", paste(keywords, collapse = "|"), ")\\b|\\bpsych\\w*")

# filtering with keywords
corpus_filtered <- corpus_id |>
  filter(grepl(pattern, text, ignore.case = TRUE))

# Saving as an csv for easier processing

write.csv(corpus_filtered, "corpus_filtered.csv", row.names = FALSE)


# Viewing unclear rows to decide whether to keep or delete them

View(corpus_id |>
       slice(42010:42015))


# creating a function that goes through all the countries and creates a csv for each country

create_country_csv <- function(country_name) {
  country_corpus <- mp_corpus(countryname == country_name, translation = "en",
                              as_tibble = TRUE)
  
  corpus_id <- country_corpus |>
    mutate(rownumber = row_number(),
           year = as.numeric(substr(date, 0, 4))) |>
    select(!c(eu_code, cmp_code, manifesto_id, annotations, translation_en))
  
  corpus_filtered <- corpus_id |>
    filter(grepl(pattern, text, ignore.case = TRUE))
  
  if (nrow(corpus_filtered) > 0) {
    write.csv(corpus_filtered, paste0("data/corpus_", country_name, ".csv"), row.names = FALSE)
  }
}


# Looping through first five countries and creating a csv for each

for (country in countries[45:50]) {
  create_country_csv(country)
  print(paste("Created CSV for", country))
}


countries[38]
# creating a new df that counts the number of rows per year

corpus_count <- corpus_filtered |>
  group_by(year) |>
  summarise(count = n()) |>
  arrange(year)
# viewing the counts per year

View(corpus_count)

# checking count of rows per party

corpus_party_count <- corpus_filtered |>
  group_by(party) |>
  summarise(count = n()) |>
  arrange(desc(count))
View(corpus_party_count)
