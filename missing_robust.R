library(ggplot2)
library(paletteer)
library(cowplot)
library(gtsummary)
library(apaTables)

summary_df <- read_csv("data/summary_df.csv",
                       col_types = cols(party_id = col_character(), 
                                        month = col_number(), MP_parfam = col_character()))

# Checking the distribution of all variables
 
 
outlier <- summary_df |>
   filter(ratio >= 0.5) |>
   select(manifesto_id) # one outlier in the ratio variable


plot <- summary_df_clean |>
  ggplot(aes(x= ratio)) +
  geom_histogram() +
  theme_minimal()

save_plot(paste("plots/descriptives/ratio_histogram.png"), plot, base_width = 6, base_height = 4)

# Counting column-wise n of missing values

missing_summary <- summary_df_clean |>
  filter(year>1998) |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing"
  )

View(missing_summary)

missing_closer <- summary_df_clean |>
  filter(year == 2007) |>
  group_by(partyname) |>
  summarise(
    missing_lrecon = sum(is.na(lrecon)),
    missing_lrgen = sum(is.na(lrgen)),
    missing_galtan = sum(is.na(galtan)),
    missing_redistribution = sum(is.na(redistribution))
  )



View(missing_closer)

# Creating a descriptives table for the variables of interest

parfams <- summary_df |> 
  distinct(MP_parfam) |> 
  pull(MP_parfam) |>
  sort()


summary_df_clean <- summary_df |>
  filter_out(ratio >= 0.5)  |> 
  mutate(MP_parfam = factor(
    MP_parfam,
    levels = parfams,
    labels = c("ECO", "LEFT", "SOSDEM", "LIB", "CHR", "CON", "NAT", "AGR", "ETH")
  )) |>
  select(-c(MP_keynesian, MP_welfare_lim, MP_minorities))

summary_table <- summary_df_clean |>
  group_by(countryname) |>
  select(-c(manifesto_id, party_id, date, partyname)) |>
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no",
    sort = list(all_categorical() ~ "frequency")
  )
    

# Saving the descriptives table as a csv file

summary_table |>
  as_tibble() |>
  write_excel_csv("data/descriptives_table.csv")


# Selecting variables for a correlation table

continuous_vars <- summary_df_clean |>
  select(year, ratio, lrecon, lrgen, galtan, redistribution, MP_pervote, MP_totseats, MP_welfare_exp)

# Creating the table, saving as a doc file
apa.cor.table(continuous_vars, 
              filename = "data/correlation_table.doc",
              show.sig.stars = TRUE,
              landscape = TRUE)

