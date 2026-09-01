library(readr)
library(ggplot2)
library(tidyr)
library(forcats)

#reading the data to R
df <- read_csv("data/country_year_counts.csv")

View(df)

range(df$count)

# UK 2005 and Ireland 2002 have to be inspected -> should not exist
# rows 921736 - 921738 UK

# rows 963608 - 963613 Ireland -> manifestos have been coded all in one cell -> 6 parties
# same w UK -> should remove these -> make a list of the ids and remove them from the id list

df <- df |>
  filter_out(country == "Ireland" & year == 2002,
             country == "United Kingdom" & year == 2005)

unique(df$country)

# Creating a stacked plot for each country where count per year is stacked

ggplot(df, aes(x = country, y = count, fill = year)) +
  geom_bar(stat = "identity") +
  labs(title = "Count per Year by Country", x = "Year", y = "Count") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

# Belgium, Germany, Netherlands, Spain have very long manifestos

ggplot(df, aes(x = year, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "manifesto length by Year", x = "Year", y = "Count") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

# calculating average number of quasi-sentences per country


average_counts <- aggregate(count ~ country, data = df, FUN = mean)
average_counts |> fct_reorder(count)
average_counts |>
  ggplot(aes(x = fct_reorder(country, count, .desc = T), y = count)) +
  geom_col() +
  geom_hline(
    yintercept = mean(average_counts$count, na.rm = TRUE),
    linetype = "dashed"
  ) +
  labs(title = "Average manifesto length by country", x = NULL, y = "N of quasi-sentences") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )


ggplot(average_counts, aes(x = country, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Avg Count per Year by Country", x = "Year", y = "Count") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

# a lot of variation in manifesto length 

# Creating a plot of number of manifestos per year in the df

yearly_counts <- df |> 
  group_by(year) |>
  summarise(count = n())


yearly_counts |> ggplot(aes(x = year, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of manifestos per year", x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

n_distinct(df$country)

