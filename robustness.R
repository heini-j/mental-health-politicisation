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
  ggplot(aes(x = year, y = ratio_log)) +
  geom_point(aes(color = translated), alpha=0.6)+
  geom_smooth(method = "loess", span = 0.9, color = "black") +
  facet_wrap(~countryname, scales = "free")+
  labs(title = NULL,
       x = NULL,
       y = "Log of ratio") +
  scale_color_paletteer_d("nbapalettes::sixers_retro")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_cowplot()



?theme_cowplot

?geom_point

save_plot("plots/translation_variance_countries.png", variance, base_width = 6, base_height = 4)


orig_lang <- summary_df |> 
  filter(translated == FALSE) |>
  filter(year >= 1975) |>
  filter(countryname == "Sweden")
  


translation <- summary_df |> 
  filter(translated == TRUE) |>
  filter(countryname == "Sweden")

fit_orig_lang <- loess(ratio_log ~ year, data = orig_lang
                    , span = 0.9
                    )
fit_translation <- loess(ratio_log ~ year, data = translation
                      , span = 0.9
                      )

orig_lang$clean_value <- residuals(fit_orig_lang)
translation$clean_value <- residuals(fit_translation)

test_df <- bind_rows(orig_lang, translation)

leveneTest(clean_value ~ as.factor(translated), data = test_df, center = median)


ggplot(test_df, aes(x = year, y = clean_value)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm") +
  facet_wrap(~ translated, scales = "free_x") +
  labs(title = "Detrended Residuals Over Time",
       x = "Year", y = "Residual Value (Cleaned)") +
  theme_minimal()

