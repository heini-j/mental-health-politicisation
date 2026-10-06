library(readr)
library(fixest)
library(broom)
library(ggfixest)
install_github("s3alfisc/fwildclusterboot")

summary_df <- read_csv("data/clean_summary.csv")

model_data <- summary_df |>
  filter_out(countryname == "Finland" & translated == FALSE)


summary_df |>
  filter(MP_parfam == "CHR") |>
  select(partyname, countryname) |>
  distinct(partyname, countryname)


model_data2 <- summary_df |>
  filter(year > 1998)



model_1 <- feols(ratio ~ year |  countryname, data = model_data)


m1 <- etable(model_1)

write_excel_csv(m1, "results/model_1.csv")

plot(model_1)

residuals(model_1) |> hist()


model_2 <- feols(ratio ~ year + MP_rightleft | countryname, data = model_data, cluster = "countryname")

etable(model_1, model_2)



summary(model_2)

residuals(model_2) |> hist()

stargazer(m1)

# Party family effects

model_3 <- feols(ratio ~  MP_parfam | year + countryname, data = model_data)

summary(model_3)

residuals(model_3) |> hist()

model_4 <- feols(ratio ~  MP_parfam + MP_totseats | year + countryname, data = model_data)

summary(model_4) 

residuals(model_4) |> hist()

model_5 <- feols(ratio ~  MP_parfam + lrecon + MP_totseats | year + countryname, data = model_data)

summary(model_5) 

residuals(model_5) |> hist()

table_test <- etable(model_3, model_4, model_5,
       digits = 3)

write_excel_csv2(table_test, "results/model_parfam.csv")


# Manifesto content variables

model_6 <- feols(ratio ~  MP_welfare_exp + MP_law | year + countryname, data = model_data)

summary(model_6)

residuals(model_6) |> hist()

model_7 <- feols(ratio ~  MP_welfare_exp + MP_human_rights + MP_equality + MP_labourgroups + MP_totseats + MP_pervote | year + countryname, data = model_data)


residuals(model_7) |> hist()
summary(model_7)

table_3 <- etable(model_6, model_7, digits = 3)

write_excel_csv2(table_3, "results/model_manifesto_content.csv")

# Ideology

model_8 <- feols(ratio ~  MP_rightleft  | year + countryname, data = model_data)

summary(model_8)

# with CHES instead

model_7 <- feols(ratio ~  lrgen  + lrecon + redistribution + spendvtax | year + countryname, data = model_data)

summary(model_7)
