library(manifestoR)
library(dplyr)
library(stringr)
library(readr)

# Setting the API key
mp_setapikey("manifesto_apikey.txt")

# Connecting to the API --------------

mp_maindataset()

full_corpus <- mp_corpus_df_bilingual(apikey="manifesto_apikey.txt")

my_corpus_df <- mp_corpus(countryname == "Sweden", translation = "en",
                          as_tibble = TRUE)

library(readr)
MPDataset_MPDS2025a <- read_csv("MPDataset_MPDS2025a.csv")
View(MPDataset_MPDS2025a)

countries <- unique(MPDataset_MPDS2025a$countryname)

# checking which countries are included in the corpus

?mp_metadata

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

# selecting relevant columns to view the accuracy of the filtering

View(corpus_filtered |>
  select(text, rownumber))

# Exporting as an excel file for easier viewing



# deleting an irrelevant rows

corpus_filtered <- corpus_filtered |>
  filter(!rownumber %in% c(533, 634, 2868, 4775, 16503, 17633, 23340, 23341, 23716, 24861, 25669, 26954, 39951, 39952, 42239
  ))

# Viewing unclear rows to decide whether to keep or delete them

View(corpus_id |>
       slice(42010:42015))
# viewing the filtered corpus


corpus_therapy <- corpus_id |>
  filter(grepl("therapy", text, ignore.case = TRUE))

View(corpus_therapy |>
  select(text, rownumber))

# Creating a year variable from the date variable currently in YYYYMM format

corpus_filtered <- corpus_filtered |>
  mutate(year = as.integer(str_sub(date, 1, 4)))

# creating a function that goes through all the countries and creates a csv for each country




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
