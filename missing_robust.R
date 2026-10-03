library(dplyr)
library(car)
library(ggplot2)
library(paletteer)
library(cowplot)
library(gtsummary)
library(apaTables)

summary_df <- read_csv("data/summary_df.csv",
                       col_types = cols(party_id = col_character(), 
                                        month = col_number(), MP_parfam = col_character()))

# Checking the distribution of all variables


continuous_vars <- names(summary_df)[sapply(summary_df, is.numeric)]

categorical_vars <- c(summary_df$orig_language, summary_df$countryname, summary_df$MP_parfam)

continuous_vars

 create_hist <- function(variable) {
  p <- ggplot(summary_df, aes(x= .data[[variable]])) +
  geom_histogram() +
  theme_minimal()
   
   save_plot(paste0("plots/descriptives/", variable, "_histogram.png"), p, base_width = 6, base_height = 4)
 }
 
 
 create_bar <- function(variable) {
   p <- ggplot(summary_df, aes(x= .data[[variable]])) +
     geom_bar() +
     theme_minimal()
   
   save_plot(paste0("plots/descriptives/", variable, "_barplot.png"), p, base_width = 6, base_height = 4)
 }
 # looping through the columns to create histograms for each variable
 
 for (var in continuous_vars[3:length(continuous_vars)]) {
   create_hist(var)
 }
  
for (var in categorical_vars) {
  create_bar(var)
}  
 
 
outlier <- summary_df |>
   filter(ratio >= 0.5) |>
   select(manifesto_id) # one outlier in the ratio variable

parfams <- summary_df |> 
  distinct(MP_parfam) |> 
  pull(MP_parfam) |>
  sort()


summary_df_clean <- summary_df |>
  filter_out(ratio >= 0.5)  |> 
  mutate(MP_parfam = factor(
    MP_parfam,
    levels = parfams,
    labels = c("ECO", "LEFT", "SOSDEM", "LIB", "CHR", "CON", "NAT", "AGR", "ETH")
  )) |>
  select(-c(MP_keynesian, MP_welfare_lim, MP_minorities))


continuous_vars <- names(summary_df_clean)[sapply(summary_df, is.numeric)]



plot <- summary_df_clean |>
  ggplot(aes(x= ratio)) +
  geom_histogram() +
  theme_minimal()

save_plot(paste("plots/descriptives/ratio_histogram.png"), plot, base_width = 6, base_height = 4)

# Counting column-wise n of missing values

missing_summary <- summary_df_clean |>
  filter(year>1998) |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing"
  )

View(missing_summary)

missing_closer <- summary_df_clean |>
  filter(year == 2007) |>
  group_by(partyname) |>
  summarise(
    missing_lrecon = sum(is.na(lrecon)),
    missing_lrgen = sum(is.na(lrgen)),
    missing_galtan = sum(is.na(galtan)),
    missing_redistribution = sum(is.na(redistribution))
  )

# Norway 2017, "Red Party" missing; before 2009 no data
# Finland 2019 Liike nyt missing
# Finland 2003 true finns missing
# 1999 three last values are missing
# Denmark 2007: new alliance; Denmark 2015 alternative
# Denmark 2001 red-green unity list & christian peoples party missing; all parties are missing the last three
# Sweden 2002 first three values available

View(missing_closer)

# Creating a descriptives table for the variables of interest

summary_df_clean <- summary_df |>
  filter(year>1998) |>
  filter(ratio < 0.5)

summary_table <- summary_df_clean |>
  group_by(countryname) |>
  select(-c(manifesto_id, party_id, date, partyname)) |>
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no")

summary_table |>
  as_tibble() |>
  write_excel_csv("data/descriptives_table.csv")


continuous_vars <- summary_df_clean |>
  select(year, ratio, lrecon, lrgen, galtan, redistribution, MP_pervote, MP_totseats, MP_welfare_exp)

apa.cor.table(continuous_vars, 
              filename = "data/correlation_table.doc",
              show.sig.stars = TRUE,
              landscape = TRUE)

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

  