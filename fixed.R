library(readr)
library(fixest)
library(broom)
library(ggfixest)
library(plm)

summary_df <- read_csv("data/clean_summary.csv")

model_data <- summary_df |>
  filter_out(country == "Finland" & translated == FALSE) |>
  group_by(country, party_id) |> # Making a lagged variable of the MP_pervote
  arrange(year) |>
  mutate(MP_pervote_lag = lag(MP_pervote, order_by = year)) |>
  ungroup()


# First models to assess the impact of year only + country fixed + controls

model_1 <- feols(ratio_log ~ year |  country, data = model_data)

model_2 <- feols(ratio_log ~ year + MP_pervote_lag  |  country, data = model_data)

model_3 <- feols(ratio_log ~ year + MP_pervote_lag + GGE_health |  country, data = model_data)


m1 <- etable(model_1, model_2, model_3, 
             digits = "r2")

write_excel_csv(m1, "results/model_year.csv")

plot(model_3)

# Second models on ideology, fixing for year and country effects 

# Party family only

model_4 <- feols(ratio_log ~ MP_parfam | country + year, data = model_data)

summary(model_4)

# Party family + right left ideology

model_5 <- feols(ratio_log ~ MP_parfam + MP_rightleft | country + year, data = model_data)

summary(model_5)

# Adding galtan (sample size reduces)

model_6 <- feols(ratio_log ~ MP_parfam + MP_rightleft + galtan | country + year, data = model_data)

# Adding controls

model_7 <- feols(ratio_log ~ MP_parfam + MP_rightleft + galtan + MP_pervote_lag + GGE_health | country + year, data = model_data)

summary(model_7)

table_2 <- etable(model_4, model_5, model_6, model_7, digits = "r2")

write_excel_csv2(table_2, "results/model_complete.csv")

# Robustness check; replacing the MP_rightleft variable with CHES equivalents

model_7 <- feols(ratio_log ~ MP_parfam + lrgen + galtan + MP_pervote_lag + GGE_health | country + year, data = model_data)

model_8 <- model_7 <- feols(ratio_log ~ MP_parfam + lrecon + galtan + MP_pervote_lag + GGE_health | country + year, data = model_data)

summary(model_8)
table_3 <- etable(model_7, digits = "r2")

table_4 <- etable(model_8, digits = "r2")


write_excel_csv2(table_3, "results/model_robust.csv")


# Hausman test ---

# Create a unique identifier for each country-party combination 
model_data$country_party <- paste(model_data$country, model_data$party_id, sep = "_")

# Defining the formula 
form <- ratio_log ~ MP_parfam + MP_rightleft + galtan + MP_pervote_lag + GGE_health

# Run the fixed and random effects models
wi <- plm(form, data = model_data, index = c("country_party", "year"), model = "within")
re <- plm(form, data = model_data, index = c("country_party", "year"), model = "random")

# The Hausman test
phtest(wi, re)


