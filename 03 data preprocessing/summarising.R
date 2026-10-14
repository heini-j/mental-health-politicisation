library(dplyr)
library(purrr)


#' This script is for summarising the results from the LLM coded manifesto documents
#' The script forms a summary of the ratio of mh content per coded manifesto document 
#' Additionally, all identified mental health content is combined into one dataset for further analysis.
#' Lastly, all rows classified as -1 are collected to check whether the LLM missed some rows

# Mapping all the coded files ------


# finding all the coded manifesto documents in the folder of coded manifestos
files <- list.files(
  path = "~/Google Drive/My Drive/Colab Notebooks/Thesis/coded",
  pattern = "\\.csv",
  full.names = TRUE
)


# 333 files

# creating a function to parse the summary of each manifesto document

parse_summary <- function(df) {
  
  tibble(
    manifesto_id = df$manifesto_id[2], # taking the data from the second row because the first row is the header
    party = df$party[2],
    date = df$date[2],
    orig_language = df$language[2],
    translated = df$translation_en[2],
    annotations = df$annotations[2],
    total_rows = nrow(df),
    rows_classified_1 = sum(df$predicted_score == 1, na.rm = TRUE)
  )
}

# Creating a summary df -----


# Looping over all the files to create a summary of each manifesto

summaries <- lapply(files, function(file) {
  
  tryCatch({
    
    # opening each file 
    df <- read_csv(file, show_col_types = FALSE) |>
      select(text, manifesto_id, party, date, language, predicted_score, translation_en, annotations)
    
    df <- df |>
      filter(str_length(str_trim(text)) >= 2) # Some documents have empty rows that impact the summary
    
    # Extract summary
    summary <- parse_summary(df)

    return(summary)
    
    
  }, error = function(e) {
    
    message("Error with manifesto ", file, ": ", e$message)
    return(NULL)
    
  })
  
})


# Combining all the summaries into one dataframe

summary_df <- list_rbind(summaries)

View(summary_df)

# Saving the dataframe for later use

write_csv(summary_df, "data/manifestos_summary.csv")

# Collecting all mental health content ------

all_mental_health <- lapply(files, function(file) {
  
  df <- read_csv(file, show_col_types = FALSE) |>
    select(text, party, date, language, predicted_score)
  
  df |> 
    
    filter(predicted_score == 1) |> 
    mutate(
      source_file = basename(file)
    )
})

all_mental_health <- list_rbind(all_mental_health)
  
View(all_mental_health)

#4402 rows


# Save the result
write_csv(all_mental_health, "data/all_mental_health.csv")

# Uncoded rows ------


# Checking if the LLM failed to code some rows

uncoded <- lapply(files, function(file) {
  
  df <- read_csv(file, show_col_types = FALSE) |>
    select(manifesto_id, text, party, date, language, predicted_score)
  
  df |> 
    
    filter(predicted_score == -1 | is.na(predicted_score)) |> 
    mutate(
      source_file = basename(file)
    )
})

uncoded <- list_rbind(uncoded) |>
  group_by(manifesto_id) |>
  summarise(
    uncoded_rows = n()
  )


View(uncoded) # 13 rows that were not coded

write_csv(uncoded, "data/uncoded_rows.csv")

