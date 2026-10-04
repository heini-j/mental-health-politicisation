library(ggplot2)
library(paletteer)
library(cowplot)
library(gtsummary)
library(apaTables)

summary_df <- read_csv("data/clean_summary.csv",
                       col_types = cols(party_id = col_character(), 
                                        month = col_number(), MP_parfam = col_character()))


# Counting column-wise n of missing values

missing_summary <- summary_df|>
  filter(year>1998) |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing"
  )

View(missing_summary)


# Checking where the missing variables are

missing_closer <- summary_df |>
  filter(year >1998) |>
  group_by(countryname, year) |>
  summarise(
    missing_lrecon = sum(is.na(lrecon)),
    missing_lrgen = sum(is.na(lrgen)),
    missing_galtan = sum(is.na(galtan)),
    missing_redistribution = sum(is.na(redistribution))
  )



View(missing_closer)

missing_parfam <- summary_df |>
  filter(year >1998) |>
  group_by(MP_parfam) |>
  summarise(
    missing_lrecon = sum(is.na(lrecon)),
    missing_lrgen = sum(is.na(lrgen)),
    missing_galtan = sum(is.na(galtan)),
    missing_redistribution = sum(is.na(redistribution))
  )

View(missing_parfam)


# Missing values come from individual (small/new) parties that were not included in the CHES survey.
# A bit higher number of missing in the "left" parfam







