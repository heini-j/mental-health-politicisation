library(manifestoR)
library(dplyr)

# Setting the API key
mp_setapikey("manifesto_apikey.txt")

# Connecting to the API --------------

mp_maindataset()


my_corpus_df <- mp_corpus(countryname == "Austria" & edate > as.Date("2000-01-01"), translation = "en",
                          as_tibble = TRUE)

# adding row numbers to help identifying documents later
corpus_id <- my_corpus_df |>
  mutate(rownumber = row_number())

# creating a list of relevant keywords
keywords <- c("mental health", "mental illness", "depression", "anxiety", "psych", "therapy")

# filtering with keywords
corpus_filtered <- corpus_id |>
  filter(grepl(paste0("\\b",paste(keywords, collapse="\\b|\\b"),"\\b"), text))

