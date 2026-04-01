library(ggplot2)
library(dplyr)

# summarising the log

log_summary <- log_df |>
  summarise(total = n(),
            success_percentage = sum(status == 1)/total * 100,
            no_data_percentage = sum(status == 0)/total * 100)
View(log_summary)

# 25 % of manifestos dont have a translation. Checking if there are any patterns

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

ggplot(log_summary_year, aes(x = year, y = success_percentage)) +
  geom_line(group = 1) +
  geom_point() +
  labs(title = "Percentage of Manifestos with English Translation by Year",
       x = "Year",
       y = "Percentage of Success") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

ggplot(log_summary_country, aes(x = reorder(countryname, -total), y = n_success)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = total, group = 1), color = "red", linewidth = 0.8) +
  geom_point(aes(y = total), color = "red") +
  labs(title = "Number of Manifestos with English Translation by Country",
       x = "Country",
       y = "Percentage of Success") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# summarising by country by year

log_summary_country_year <- log_df |>
  group_by(countryname, year) |>
  summarise(total = n(),
            success_percentage = sum(status == 1)/total * 100,
            no_data_percentage = sum(status == 0)/total * 100)
View(log_summary_country_year)

log_summary_country_year$year <- as.numeric(log_summary_country_year$year)

# visualising the data availability per country per year

ggplot(log_summary_country_year, aes(x = year, y = success_percentage, group = countryname)) +
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

# ~2010 seems to be the year after which most countries have a good coverage of english translations
