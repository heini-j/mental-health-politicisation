library(readr)
library(ggplot2)
library(dplyr)
library(cowplot)

# reading the data to R -----

log_df <- read_csv("data/log_df.csv")

# summarising the stats ----

# availability of english translations in the whole dataset

log_summary <- log_df |>
  summarise(total = n(),
            success_percentage = sum(status == 1)/total * 100,
            no_data_percentage = sum(status == 0)/total * 100)
View(log_summary)

# 25 % of manifestos dont have a translation

# Checking for patterns ----

# creating a year variable by separating the first 4 digits of the date variable

log_df <- log_df |>
  mutate(year = str_sub(date, 1, 4))

View(log_df)

log_summary_year <- log_df |>
  group_by(year) |>
  summarise(total = n(),
            success_percentage = sum(status == 1)/total * 100,
            no_data_percentage = sum(status == 0)/total * 100)

View(log_summary_year)

# visualising the data availability per year

year_availablity <- ggplot(log_summary_year, aes(x = year, y = success_percentage)) +
  geom_line(group = 1) +
  geom_point() +
  labs(title = "Percentage of Manifestos with English Translation by Year",
       x = "Year",
       y = "Percentage of Success") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/year_availability.png", year_availablity, width = 12, height = 8)

# earlier years have fewer english translations available -> 2005 is last year when total went below 50 %

# summarising by country

log_summary_country <- log_df |>
  group_by(countryname) |>
  summarise(total = n(),
            n_success = sum(status == 1),
            success_percentage = sum(status == 1)/total * 100,
            n_no_data = sum(status == 0),
            no_data_percentage = sum(status == 0)/total * 100)

View(log_summary_country)

# visualising the data availability per country

country_availability <- ggplot(log_summary_country, aes(x = reorder(countryname, -total), y = n_success)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = total, group = 1), color = "red", linewidth = 0.8) +
  geom_point(aes(y = total), color = "red") +
  labs(title = "Number of Manifestos with English Translation by Country",
       x = "Country",
       y = "Percentage of Success") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/country_availability.png", country_availability, width = 12, height = 8)

# croatia and malta don't have any english translations


# summarising by country by year

log_summary_country_year <- log_df |>
  group_by(countryname, year) |>
  summarise(total = n(),
            success_percentage = sum(status == 1)/total * 100,
            no_data_percentage = sum(status == 0)/total * 100)
View(log_summary_country_year)

log_summary_country_year$year <- as.numeric(log_summary_country_year$year)

# visualising the data availability per country per year

year_per_country <- ggplot(log_summary_country_year, aes(x = year, y = success_percentage, group = countryname)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ countryname) +
  geom_hline(yintercept = 50, color = "red", linewidth = 0.5) +
  scale_x_continuous(
    breaks = seq(min(log_summary_country_year$year, na.rm = TRUE),
                 max(log_summary_country_year$year, na.rm = TRUE),
                 by = 5)
  ) +
  labs(title = "Percentage of Manifestos with English Translation by Country and Year",
       x = "Year",
       y = "Percentage of Success") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/year_per_country.png", year_per_country, width = 12, height = 8)

# ~2010 seems to be the year after which most countries have a good coverage of english translations

# Creating an updated list of manifesto ids to use for data retrieval ----

# Picking only those with english translation available, leaves a decent sample of 923

manifesto_ids_updated <- log_df |>
  filter(status == 1) |>
  select(party, date, countryname)

# Saving the updated list of manifesto ids for later use

write_csv(manifesto_ids_updated, "data/manifesto_ids.csv")


