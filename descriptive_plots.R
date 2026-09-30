library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(paletteer)
library(car)
library(cowplot)


# NOtes: country has NAs, there is one outlier in the ratio variable (Sweden 1964) -> will be removed in the analysis.
# opposition vs government pov

summary_df <- read_csv("data/summary_df.csv")

View(summary_df)

summary_df <- summary_df |>
  filter_out(ratio > 10) # removing the outlier in the ratio variable (Sweden 1964)


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
  group_by(countryname, year) |>
  summarise(n_rows = mean(total_rows),
            n_mh = mean(rows_classified_1),
            n_documents = n(),
            .groups = "drop")

View(rows_summary)

summary_plot <- rows_summary |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = n_rows, color = "Total n of quasi-sentences")) +
  geom_point(aes(y = n_rows, color = "Total n of quasi-sentences")) +
  geom_text(
    aes(y = n_rows, label = n_documents),
    nudge_y = 40,
    nudge_x = -0.5,
    size = 4
  )+
  geom_line(aes(y = n_mh*10, color = "Mental health references")) +
  scale_color_paletteer_d("futurevisions::jupiter") +
  facet_wrap(~countryname, scales = "free") +
  labs(title = NULL,
       x = NULL,
       y = "Number of quasi-sentences") +
  scale_x_continuous(
    breaks = seq(min(rows_summary$year),
                 max(rows_summary$year),
                 by = 10)) +
  scale_y_continuous(
    sec.axis = sec_axis(
      ~ . / 10,
      name = "References to mental health")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))


save_plot("plots/document_summary.png", summary_plot, base_height = 10, base_width = 16)

  
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
  #filter(year > 1998) |>
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
  group_by(year, countryname, MP_parfam, partyname) |>
  summarise(avg_ratio = mean(ratio))

summary_years |> 
  filter(countryname == "Finland") |>
  #filter(year > 1998) |>
  ggplot(aes(x=year, y = avg_ratio)) +
  #geom_line()+
  #geom_smooth(method = "loess", se = T, color = "blue") +
  geom_point(alpha = 0.6)+
  #facet_wrap(~countryname)+
  scale_y_continuous(limits = c(0,6)) +
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





df_method1 <- summary_df |> filter(countryname == "Sweden", translated == TRUE, year < 2015)
df_method2 <- summary_df |> filter(countryname == "Sweden", translated == FALSE, year >1985)

fit1 <- loess(ratio ~ year, data = df_method1, span = 0.9)
fit2 <- loess(ratio ~ year, data = df_method2, span = 0.9)

df_method1$clean_value <- residuals(fit1)
df_method2$clean_value <- residuals(fit2)

df_clean <- bind_rows(df_method1, df_method2)

leveneTest(clean_value ~ as.factor(translated), data = df_clean, center = median)


ggplot(df_clean, aes(x = year, y = clean_value, color = translated)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm") +
  facet_wrap(~ translated, scales = "free_x") +
  labs(title = "Detrended Residuals Over Time",
       x = "Year", y = "Residual Value (Cleaned)") +
  theme_minimal()
