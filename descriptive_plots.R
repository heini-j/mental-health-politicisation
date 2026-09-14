library(readr)
library(tidyr)
library(ggplot2)
library(paletteer)


# NOtes: country has NAs
# opposition vs government pov

summary_df <- read_csv("data/summary_df.csv")

View(summary_df)

manifesto_ids <- read_csv("data/manifesto_ids_nordic.csv")

View(manifesto_ids)

manifesto_ids <- manifesto_ids |>
  mutate(
    year =  as.integer(substr(date, 1, 4)),
    month = substr(date, 5,6))


# Creating a table showing the number of manifesto documents by country by year


document_summary <- manifesto_ids |>
  group_by(countryname, year) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(year) |>
  pivot_wider(
    names_from = countryname,
    values_from = n,
    values_fill = 0
  ) 

View(document_summary)

write_excel_csv(document_summary, "data/manifestos_summary.csv")

# summarising the total rows per year per country

rows_summary <- summary_df |>
  group_by(countryname, year) |>
  summarise(n_rows = mean(total_rows), .groups = "drop")

View(rows_summary)

summary_plot <- rows_summary |> ggplot(aes(x = year, y = n_rows, color = countryname)) +
  geom_line() +
  geom_point(size = 2) + 
  labs(title = "Average length of manifestos in the sample",
       x = NULL,
       y = "Number of quasi-sentences") +
  theme_minimal() +
  scale_color_paletteer_d("futurevisions::jupiter") +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# plotting the ratio over time 

parfams <- summary_df |> 
  distinct(parfam) |> 
  pull(parfam) |>
  sort()



summary_df <- summary_df |>
  mutate(parfam = factor(
    parfam,
    levels = parfams,
    labels = c("ECO", "LEFT", "SOSDEM", "LIB", "CHR", "CON", "NAT", "AGR", "ETH")
  ))

summary_df |> ggplot(aes(x = year, y = ratio)) +
  geom_line() +
  facet_wrap(~parfam)+
  labs(title = "x",
       x = "Date",
       y = "Ratio of Classified Rows") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")


# Plotting the average ratio over years in a line plot

summary_years <- summary_df |>
  group_by(year, parfam) |>
  summarise(avg_ratio = mean(ratio))

summary_years |> ggplot(aes(x=year, y = avg_ratio)) +
  geom_line()+
  geom_point()+
  labs(title = "x",
       x = NULL,
       y = "Average ratio of Classified Rows") +
  theme_minimal() 

# regression analysis 

model <- lm(ratio ~ year + countryname + year * countryname, data = summary_df)

summary(model)

# making a scatter plot of the ratio and lrgen variables

ggplot(summary_df, aes(x=seat, y = ratio)) +
  geom_point() +
  geom_smooth(method = "lm", se = T, color = "blue") +
  facet_wrap(~countryname)

