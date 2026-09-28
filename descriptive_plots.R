library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(paletteer)



# NOtes: country has NAs
# opposition vs government pov

summary_df <- read_csv("data/summary_df.csv")

View(summary_df)

# Creating a table showing the number of manifesto documents by country by year


document_summary <- summary_df |>
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
  group_by(countryname, year, translated) |>
  summarise(n_rows = mean(total_rows), .groups = "drop")

View(rows_summary)

summary_plot <- rows_summary |> ggplot(aes(x = year, y = n_rows, color = translated)) +
  geom_line() +
  geom_point(size = 2) + 
  facet_wrap(~countryname) +
  labs(title = "Average length of manifestos in the sample",
       x = NULL,
       y = "Number of quasi-sentences") +
  scale_x_continuous(
    breaks = seq(min(rows_summary$year),
                 max(rows_summary$year),
                 by = 10)) +
  scale_y_continuous(
    breaks = seq(100,
                 3000,
                 by = 500)) +
  theme_minimal() +
  scale_color_paletteer_d("futurevisions::jupiter") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
  
  
# plotting the ratio over time 

parfams <- summary_df |> 
  distinct(MP_parfam) |> 
  pull(MP_parfam) |>
  sort()



summary_df <- summary_df |>
  mutate(MP_parfam = factor(
    MP_parfam,
    levels = parfams,
    labels = c("ECO", "LEFT", "SOSDEM", "LIB", "CHR", "CON", "NAT", "AGR", "ETH")
  ))

summary_df |> 
  #filter(translated == F) |>
  filter(year > 1998) |>
  ggplot(aes(x = year, y = ratio)) +
  geom_point() +
  facet_wrap(~countryname) + 
  geom_smooth(method = "lm") +
  labs(title = "x",
       x = "Date",
       y = "Ratio of Classified Rows") +
  scale_y_continuous(limits = c(0,6)) +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")


# Plotting the average ratio over years in a line plot

summary_years <- summary_df |>
  group_by(year, countryname, MP_parfam) |>
  summarise(avg_ratio = mean(ratio))

summary_years |> 
  #filter_out(countryname == "Sweden") |>
  filter(year > 1998) |>
  ggplot(aes(x=year, y = avg_ratio)) +
  geom_point()+
  geom_smooth(method = "lm", se = T, color = "blue") +
  #geom_point()+
  facet_wrap(~countryname)+
  labs(title = "x",
       x = NULL,
       y = "Average ratio of Classified Rows") +
  theme_minimal() 

# regression analysis 

model <- lm(ratio ~ year + countryname + year * countryname, data = summary_df)

summary(model)

# making a scatter plot of the ratio and lrgen variables

summary_df |>
  filter(translated == T) |>
  ggplot(aes(x=lrecon, y = ratio)) +
  geom_point() +
  #facet_wrap(~countryname) +
  geom_smooth(method = "lm", se = T, color = "blue") +
  scale_y_continuous(limits = c(0,6))

