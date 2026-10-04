library(dplyr)
library(readr)
library(ggplot2)
library(car)
library(paletteer)


summary_df <- read_csv("data/clean_summary.csv",
                       col_types = cols(party_id = col_character(), 
                                        month = col_number(), MP_parfam = col_character()))

# LOESS estimation for robustness to see if translated vs non-translated documents have different variation 
variance <- summary_df |>
  #filter(countryname == "Finland") |>
  filter(year >= 1999) |>
  ggplot(aes(x = year, y = ratio
             #, color = translated
  )) +
  geom_point(alpha=0.7)+
  geom_smooth(method = "lm") +
  facet_wrap(~countryname)+
  #geom_vline(xintercept = 1970)+
  labs(title = NULL,
       x = NULL,
       y = "ratio") +
  theme_minimal() +
  scale_color_paletteer_d("nbapalettes::sixers_retro")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

save_plot("plots/translation_variance.png", variance, base_width = 6, base_height = 4)


df_method1 <- summary_df |> 
  #filter(countryname == "Finland") |>
  filter(translated == FALSE) #|>
  mutate("method" = 1)


df_method2 <- summary_df |> 
  #filter(countryname %in% c("Denmark", "Sweden", "Norway")) |> 
  filter(translated == TRUE
         #,year >= 1995
  ) |>
  mutate("method" = 2)

fit1 <- loess(ratio ~ year, data = df_method1, span = 0.9)
fit2 <- loess(ratio ~ year, data = df_method2, span = 0.9)

df_method1$clean_value <- residuals(fit1)
df_method2$clean_value <- residuals(fit2)

df_clean <- bind_rows(df_method1, df_method2)

leveneTest(clean_value ~ as.factor(translated), data = df_clean, center = median)


ggplot(df_clean, aes(x = year, y = clean_value)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm") +
  facet_wrap(~ translated, scales = "free_x") +
  labs(title = "Detrended Residuals Over Time",
       x = "Year", y = "Residual Value (Cleaned)") +
  theme_minimal()

