library(readr)
library(fixest)

model_data <- read_csv("data/clean_summary.csv")

# Models 3 & 4 ----

# Negative binomial regression with fixed effects for year and country

model_3 <- fenegbin(rows_classified_1 ~ MP_rightleft | year + country, data = model_data)

summary(model_3)

# Checking the IRR

exp(coef(model_3))

exp(confint(model_3))

# Adding the control variablel
model_4 <- fenegbin(rows_classified_1 ~ MP_rightleft + MP_pervote_lag | year + country, data = model_data)

summary(model_4)

# Checking the IRR

exp(coef(model_4))

exp(confint(model_4))

# Combining the tables and saving them as a csv file

table_2 <- etable(model_3, model_4, digits = "r2")

write_excel_csv2(table_2, "results/model_complete.csv")



# Models 5, 6 and 7 ----

# Taking a subset of data which includes more parties + CHES scores


model_data_2 <- model_data |>
  filter(year > 1998)


# Repreating  model 4 to check if results still apply


model_7 <- fenegbin(rows_classified_1 ~ MP_rightleft + MP_pervote_lag | country + year, data = model_data_2)

summary(model_7)

exp(coef(model_7))

exp(confint(model_7))

# Adding the galtan variable to the model

model_8 <- fenegbin(rows_classified_1 ~ MP_rightleft + galtan + MP_pervote_lag | country + year, data = model_data_2)

summary(model_8)

exp(coef(model_8))

exp(confint(model_8))

# Adding the GGE_health control variable to the model

model_9 <- fenegbin(rows_classified_1 ~ MP_rightleft + galtan + MP_pervote_lag + GGE_health | country + year, data = model_data_2)

summary(model_9)

exp(coef(model_9))

exp(confint(model_9))

# Checking results with CHES left-right variable 

model_10 <- fenegbin(rows_classified_1 ~ lrgen + galtan + MP_pervote_lag + GGE_health | country + year, data = model_data_2)

summary(model_10)

exp(coef(model_10))

exp(confint(model_10))

# Combining the tables and saving them as a csv file
table_3 <- etable(model_8, model_9, model_10, digits = "r2")

write_excel_csv2(table_3, "results/model_complete2.csv")

table_4 <- etable(model_10, digits = "r2")


write_excel_csv2(table_4, "results/model_lrecon.csv")
