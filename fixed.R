library(readr)
library(fixest)
library(ggfixest)
library(plm)
library(modelsummary)
library(gtsummary)


model_data <- read_csv("data/clean_summary.csv")



# Hausman test ---


# first models 

# Create a unique identifier for each country-party combination 
model_data$country_party <- paste(model_data$country, model_data$party_id, sep = "_")

# Defining the formula 
form <- mh_log ~ MP_rightleft + MP_pervote_lag 

# Run the fixed and random effects models
wi <- plm(form, data = model_data, index = c("country_party", "year"), model = "within")
re <- plm(form, data = model_data, index = c("country_party", "year"), model = "random")

# The Hausman test
phtest(wi, re)


# Newer data with more parties and more variables
model_data_2 <- model_data |>
  filter(year >= 1999)


# Create a unique identifier for each country-party combination 
model_data_2$country_party <- paste(model_data_2$country, model_data_2$party_id, sep = "_")

# Defining the formula 
form <- mh_log ~ MP_rightleft + galtan + MP_pervote_lag + GGE_health

# Run the fixed and random effects models
wi <- plm(form, data = model_data_2, index = c("country_party", "year"), model = "within")
re <- plm(form, data = model_data_2, index = c("country_party", "year"), model = "random")

# The Hausman test
phtest(wi, re)

# OLS ---


# First OLS models to assess the impact of year only + country fixed + controls


model_year <- lm(mh_log ~ year, data = model_data)

model_w_control <- lm(mh_log ~ year +  MP_pervote_lag, data = model_data)

modelsummary(list("1" = model_year, "2" = model_w_control), 
             output = "results/regression_table.docx",
             fmt = fmt_decimal(digits = 2, pdigits = 3),
             statistic = "p.value",
             stars = TRUE)



summary(model_year)

# Fixed effects

# Second models on ideology, fixing for year and country effects 

# Party family only

model_4 <- feols(mh_log ~ MP_rightleft | year, data = model_data)

summary(model_4)

# Adding control

model_5 <- feols(mh_log ~ MP_rightleft + MP_pervote_lag | year, data = model_data)

summary(model_5)

plot(model_5, type = "coef")

table_2 <- etable(model_4, model_5, digits = "r2")

write_excel_csv2(table_2, "results/model_complete.csv")

# With newer data and more parties

# Repreating the same model with newer data
model_7 <- feols(mh_log ~ MP_rightleft + MP_pervote_lag | country + year, data = model_data_2)

summary(model_7)


model_8 <- feols(mh_log ~ MP_rightleft + galtan + MP_pervote_lag | country + year, data = model_data_2)

summary(model_8)



model_9 <- feols(mh_log ~ MP_rightleft + galtan + MP_pervote_lag + GGE_health | country + year, data = model_data_2)

summary(model_9)


model_10 <- feols(mh_log ~ lrecon + galtan + MP_pervote_lag + GGE_health | country + year, data = model_data_2)

summary(model_10)


table_3 <- etable(model_7, model_8, model_9, digits = "r2")

write_excel_csv2(table_3, "results/model_complete2.csv")




