library(readr)
library(ggplot2)

# reading the data to R

country_year_counts <- read_csv("data/country_year_counts.csv",
                                col_select = c("country", "year", "count"))

View(country_year_counts)

length(unique(country_year_counts$country)) # 39 countries

sum(country_year_counts$count)


# Plotting the data

ggplot(country_year_counts, aes(x = year, y = count)) +
  geom_col() +
  #facet_wrap(~country) +
  labs(
    title = "Number of Rows by Country Over Time",
    x = "Year",
    y = "Number of Publications"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Big differences in total number of quasi sentences per countries
# an increase in quasi-sentences until 2019, after decrease -> maybe not so many manifestos after?


