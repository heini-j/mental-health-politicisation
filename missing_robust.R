library(dplyr)
library(car)
library(ggplot2)
library(paletteer)
library(cowplot)

summary_df <- read_csv("data/summary_df.csv")


summary_df <- summary_df |>
  filter_out(ratio > 10) |>
  select(-c(womens_rights, spendvtax_salience, redist_salience, ches, partyfacts_id))

sum(summary_df$total_rows)

?select
names(summary_df)


summary_df |>
  filter(countryname == "Finland", year > 1998) |>
  ggplot(aes(x = year, y = lrgen, color = partyname)) +
  geom_point()

ggplot(summary_df, aes(x= govt)) +
  geom_bar(width = 0.8, na.rm=T)


# finland 2015 and denmark 2015 elections dont have ches scores, norway only 2009 -> should it be inputated or not?




?geom_histogram


# Counting column-wise n of missing values

missing_summary <- summary_df |>
  filter(year>1998) |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing"
  )

View(missing_summary)

missing_countries <- summary_df |>
  filter(year> 1998, countryname=="Finland")|>
  group_by(year) |>
  #group_by(countryname, year) |>
  #filter(year > 1998) |>
  summarise(
    missing_lrecon = sum(is.na(lrecon)),
    missing_lrgen = sum(is.na(lrgen)),
    missing_galtan = sum(is.na(galtan)),
    missing_redistribution = sum(is.na(redistribution)),
    missing_social = sum(is.na(sociallifestyle)),
    missing_regions = sum(is.na(regions))
  )

# Norway 2017, "Red Party" missing; before 2009 no data
# Finland 2019 Liike nyt missing
# Finland 2003, 2007 & 2011 all values there
# 1999 three last values are missing
# Denmark 2001 red-green unity list & christian peoples party missing; all parties are missing the last three
# Denmark 2005 centre democrats and christian democrats are missing
# Denmark 2007, 2011 and 2019 all values are there
# Sweden 2002 first three values available
# 2006-2018 all values available; 2022 social lifestyle not measured anymore

View(missing_countries)

# Finland & Denmark 2015 missing all the ches variables

# Almost all or > 50 % are missing womens_rights & spendvtax_salience, redist_salience  -> remove these
# govt, lrgen, lrecon, galtan, galtan, redistribution, sociallifestyle, regions ´have some missing -> analyse this


# Creating a correlation table for the missing variables





# VIsualising all variables for checks




# LOESS estimation for robustness to see if translated vs non-translated documents have different variation 
variance <- summary_df |>
  filter(countryname == "Finland") |>
  filter(year >= 1960) |>
  ggplot(aes(x = year, y = ratio
             #, color = translated
             )) +
  geom_point(alpha=0.7)+
  geom_smooth(method = "lm") +
  #facet_wrap(~countryname)+
  #geom_vline(xintercept = 1970)+
  labs(title = NULL,
       x = NULL,
       y = "% of quasi-sentences") +
  theme_minimal() +
  scale_color_paletteer_d("nbapalettes::sixers_retro")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

save_plot("plots/translation_variance.png", variance, base_width = 6, base_height = 4)


df_method1 <- summary_df |> 
  filter(countryname == "Finland") |>
  filter(translated == TRUE) |>
  mutate("method" = 1)


df_method2 <- summary_df |> 
  filter(countryname %in% c("Denmark", "Sweden", "Norway")) |> 
  filter(translated == TRUE
         #,year >= 1995
         ) |>
  mutate("method" = 2)

fit1 <- loess(ratio ~ year, data = df_method1, span = 0.9)
fit2 <- loess(ratio ~ year, data = df_method2, span = 0.9)

df_method1$clean_value <- residuals(fit1)
df_method2$clean_value <- residuals(fit2)

df_clean <- bind_rows(df_method1, df_method2)

leveneTest(clean_value ~ as.factor(method), data = df_clean, center = median)


ggplot(df_clean, aes(x = year, y = clean_value)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm") +
  facet_wrap(~ method, scales = "free_x") +
  labs(title = "Detrended Residuals Over Time",
       x = "Year", y = "Residual Value (Cleaned)") +
  theme_minimal()

  