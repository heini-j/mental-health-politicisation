library(readr)
library(ggplot2)
library(dplyr)
library(purrr)

# reading the data to R ------

# Summary data for the LLM scores of the manifesto documents

summary <- read_csv("data/summary_nordic.csv")

View(summary)

# Adding some key variables

summary <- summary |>
  mutate(
    party = as.character(substr(manifesto_id, 1,5)),
    date = substr(manifesto_id, 7, 12),
    year =  as.integer(substr(manifesto_id, 7, 10)),
    month = substr(manifesto_id, 11,12),
    ratio = rows_classified_1/total_rows*100
  )


parties <- summary |>
  select("party") |>
  distinct() |>
  pull(party)

# 38 parties

# The manifesto project main dataset to get the party families and names

manifesto <- read_csv("data/MPDataset_MPDS2026a.csv",
                      col_types = cols(edate = col_character(), 
                                       parfam = col_character())) |>
  select(c("date", "party", "partyname", "parfam")) |> # We don't need all the columns
  filter(party %in% parties) |>
  mutate(party = as.character(party))


View(manifesto)

# joining the ches to the df using the partyfacts dataset

partyfacts <- 
  read_csv("data/partyfacts-external-parties.csv", locale = locale(encoding = "UTF-8"),
           col_types = cols(partyfacts_id = col_character())) |> 
  filter(dataset_key %in% c("ches", "manifesto"),
         country %in% c("SWE", "FIN", "NOR", "DNK")) |> # we're only interested in these two datasets
           # filtering for Sweden only for this test run
           select(partyfacts_id,
                  dataset_key,
                  dataset_party_id,
                  year_first,
                  year_last,
                  name_short,
                  country) 

# Loading the CHES scores and renaming to match with the corpus df
ches <- read_csv("data/1999-2024_CHES_dataset_meansV2.csv") |> 
  rename(ches = party_id) |>
  mutate(ches = as.character(ches))
  
# Counting how many times "ches" is mentioned in the dataset_key column

sum(partyfacts$dataset_key == "ches", na.rm = TRUE) # 34 should have ches
sum(partyfacts$dataset_key == "manifesto", na.rm = TRUE) # only 4 have manifesto



bilingual_names <- partyfacts|> 
  group_by(partyfacts_id) |>
  summarise(
    name_bilingual = paste(unique(name_short), collapse = " / ")
  )

bilingual_names <- left_join(bilingual_names, partyfacts, by = "partyfacts_id")


# sequencing to go from first and last year to s year-level data

partyfacts_years <- bilingual_names |> 
  group_by(rn = row_number()) |>
  mutate(year = list(year_first:year_last)) |>
  unnest(cols = c(year)) |>
  ungroup() |>
  select(partyfacts_id,
         dataset_key,
         dataset_party_id,
         year,
         name_bilingual)


# Saving some columns to combine later with the wide dataset

columns_keep <- partyfacts_years |> 
  select(partyfacts_id, name_bilingual)

# pivoting wider only for the dataset party ids

partyfacts_wider <- partyfacts_years |>
  pivot_wider(id_cols = c(partyfacts_id, year),
              id_expand = F,
              names_from = dataset_key,
              values_from = dataset_party_id) |>
  filter(year > 1998)


partyfacts_wider <- partyfacts_wider |>
  group_by(partyfacts_id) |>
  mutate(
    manifesto = first(na.omit(manifesto)),
    ches = first(na.omit(ches))
  ) |>
  ungroup()

# removing lines where either ches or manifesto is NA

partyfacts_final <- partyfacts_wider |>
  filter_out(is.na(manifesto) | is.na(ches)) |>
  rename("party" = manifesto)


# Combining the datasets ------

# Adding partyfacts ids to the summary

summary_df <- left_join(summary, partyfacts_final, by = c("party", "year"))

summary_complete <- inner_join(summary_df, ches, by = c("ches", "year")) |>
  rename("party"=party.x) |>
  mutate(date = substr(manifesto_id, 7, 12))

summary_complete$date <- as.double(summary_complete$date)

summary_complete_final <- left_join(summary_complete, manifesto, by = c("party", "date"))

View(summary_complete_final)

# plotting the ratio over time 

summary_df$parfam <- as.character(summary_df$parfam)

summary_complete_final |> ggplot(aes(x = year, y = ratio, color= parfam, group = parfam)) +
  geom_line() +
  labs(title = "x",
       x = "Date",
       y = "Ratio of Classified Rows") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")




