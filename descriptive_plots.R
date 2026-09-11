library(readr)
library(tidyr)
library(ggplot2)


# NOtes: country has NAs
# opposition vs government pov

summary_df <- read_csv("data/summary_df.csv")

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

?geom_smooth
