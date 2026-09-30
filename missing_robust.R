library(dplyr)
library(car)
library(ggplot2)

summary_df <- read_csv("data/summary_df.csv")

summary_df <- summary_df |>
  filter_out(ratio > 10) 


# Counting column-wise n of missing values

missing_summary <- summary_df <-
  summarise(across(all_of(cols), ~sum(is.na(x)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing"
  )

# VIsualising all variables for checks




# LOESS estimation for robustness to see if translated vs non-translated documents have different variation 
summary_df |>
  ggplot(aes(x = year, y = ratio, color = translated)) +
  geom_point(alpha=0.7)+
  geom_smooth(method = "loess", se = F, span=0.9) +
  facet_wrap(~countryname)+
  #geom_vline(xintercept = 1970)+
  labs(title = "Spread of ratio for translated and non-translated documents",
       x = NULL,
       y = "% of quasi-sentences")

df_method1 <- summary_df |> filter(countryname == "Finland", translated == TRUE)
df_method2 <- summary_df |> filter(countryname == "Finland", translated == FALSE, year >= 1990)

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
