library(readr)
library(tidyr)

summary_df <- read_csv("data/summary_df.csv",
                       col_types = cols(party_id = col_character(), 
                                        month = col_number(), MP_parfam = col_character()))

uncoded_rows <- read_csv("data/uncoded_rows.csv") # file with information on rows that we not coded by LLM


# Outliers ----

# There is one outlier in ratio variable, which is a Swedish manifesto from 1964. We remove it.

# Saving it
outlier <- summary_df |>
  filter(ratio >= 0.5) |>
  select(manifesto_id) # one outlier in the ratio variable


summary_df_clean <- summary_df |>
  left_join(uncoded_rows, by = "manifesto_id") |>
  mutate(uncoded_rows = ifelse(is.na(uncoded_rows), 0, uncoded_rows),
         total_rows = total_rows - uncoded_rows) |>
  select(-uncoded_rows) |>
  mutate(ratio = rows_classified_1/total_rows,
         lrgen = 10- lrgen,
         lrecon = 10 - lrecon, 
         redistribution = 10- redistribution,
         spendvtax = 10 - spendvtax)
         |> # reversing the right left to match with the CHES variables
  filter(ratio < 0.5) # removing one outlier in the ratio variable


write_csv(summary_df_clean, "data/clean_summary.csv")






