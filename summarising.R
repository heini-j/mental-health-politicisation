library(dplyr)
library(purrr)


#' This script is for retrieving all mental health content from the LLM classified manifestos


# finding all the coded manifesto documents in the folder
files <- list.files(
  path = "~/Google Drive/My Drive/Colab Notebooks/Thesis/coded",
  pattern = "\\.csv",
  full.names = TRUE
)

View(files)

# Combining all the mental health content from the files

all_mental_health <- map_dfr(files[160:175], function(file) {
  
  df <- read_csv(file, show_col_types = FALSE) |>
    select(text, manifesto_id, party, date, language, predicted_score)
  
  df |> 
    filter(predicted_score == 1) |> 
    mutate(
      source_file = basename(file)
    )
})

  
View(all_mental_health)
# Save the result
write_csv(all_mental_health, "all_mental_health_rows.csv")