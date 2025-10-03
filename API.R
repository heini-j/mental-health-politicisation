library(manifestoR)
library(dplyr)

# Setting the API key
mp_setapikey("manifesto_apikey.txt")

# Connecting to the API --------------

mp_maindataset()


my_corpus_df <- mp_corpus(countryname == "Austria" & edate > as.Date("2000-01-01"), translation = "en",
                          as_tibble = TRUE)
keywords <- c("mental health", "mental illness", "depression", "anxiety", "psych", "therapy")
corpus_filtered <- my_corpus_df |>
  filter(grepl(paste0("\\b",paste(keywords, collapse="\\b|\\b"),"\\b"), text))

