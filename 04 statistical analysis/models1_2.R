library(readr)
library(fixest)
library(ggfixest)
library(plm)
library(modelsummary)
library(gtsummary)
library(car)
library(rstatix)


model_data <- read_csv("data/clean_summary.csv")

# Negative binomial regression ---


# Impact of year on references 


model0 <- MASS::glm.nb(rows_classified_1 ~ year, data = model_data)

summary(model0)

# Exponentiated coefficients (IRR)
exp(coef(model0))

exp(confint(model0))

# with control variable

model_w_control <- MASS::glm.nb(rows_classified_1  ~ year +  MP_pervote_lag, data = model_data)

summary(model_w_control)

exp(coef(model_w_control))
exp(confint(model_w_control))

# Saving the regression table for later

modelsummary(list("1" = model0, "2" = model_w_control), 
             output = "results/regression_table.docx",
             fmt = fmt_decimal(digits = 2, pdigits = 3),
             statistic = "p.value",
             stars = TRUE)




# Robustness: does fixed country effects change the result

fe_nb_model <- fenegbin(rows_classified_1  ~ year +  MP_pervote_lag | country, 
                       data = model_data)

results <- etable(fe_nb_model)

write_excel_csv2(results, "results/robustness_model.csv")

exp(coef(fe_nb_model))


