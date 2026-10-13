library(dplyr)
library(readr)
library(ggplot2)
library(car)
library(paletteer)

#' This script is for testing whether translated and original language manifestos
#' were encoded equally accurately. The test is best on LOESS fitting on the language versions
#' separately and testing the residuals for equality of variance using Levene's test.
#'The script conducts the test for the Swedish manifestos, but can be adapted to the other countries
#'by changing the country name.


summary_df <- read_csv("data/clean_summary.csv",
                       col_types = cols(party_id = col_character(), 
                                        month = col_number(), MP_parfam = col_character()))

# LOESS estimation----

# Plotting the variance for visual inspection
variance <- summary_df |>
  ggplot(aes(x = year, y = mh_log)) +
  geom_point(aes(color = translated), alpha=0.6)+
  geom_smooth(method = "loess", span = 0.9, color = "black") +
  facet_wrap(~country, scales = "free")+
  labs(title = NULL,
       x = NULL,
       y = "Log of mental health references") +
  scale_color_paletteer_d("nbapalettes::sixers_retro")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_cowplot()


save_plot("plots/translation_variance_countries.png", variance, base_width = 6, base_height = 4)


# LOESS estimation for translated and original language Swedish content

# Separating the df between translated and original language content
orig_lang <- summary_df |> 
  filter(translated == FALSE) |>
  filter(year >= 1975) #|>
  filter(country == "Finland") # Using the same code by changing country name here
  
translation <- summary_df |> 
  filter(translated == TRUE) #|>
  filter(country == "Finland")

# LOESS fitting separately
fit_orig_lang <- loess(mh_log ~ year, data = orig_lang
                    , span = 0.9
                    )
fit_translation <- loess(mh_log ~ year, data = translation
                      , span = 0.9
                      )

# Separating the residuals from the fitted values to get the detrended values
orig_lang$clean_value <- residuals(fit_orig_lang)
translation$clean_value <- residuals(fit_translation)

# Combining for the Levene's test
test_df <- bind_rows(orig_lang, translation)

# Levene's test
leveneTest(clean_value ~ as.factor(translated), data = test_df, center = median)

# Visual inspection of the residuals between the groups
ggplot(test_df, aes(x = year, y = clean_value)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm") +
  facet_wrap(~ translated, scales = "free_x") +
  labs(title = "Detrended Residuals Over Time",
       x = "Year", y = "Residual Value (Cleaned)") +
  theme_minimal()

