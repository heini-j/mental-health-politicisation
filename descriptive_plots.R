library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(paletteer)
library(car)
library(cowplot)


# NOtes: country has NAs, there is one outlier in the ratio variable (Sweden 1964) -> will be removed in the analysis.
# opposition vs government pov

summary_df <- read_csv("data/summary_df.csv",
                       col_types = cols(party_id = col_character(), 
                                        month = col_number(), MP_parfam = col_character()))

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


save_plot("plots/document_summary.png", summary_plot, base_height = 6, base_width = 10)

  
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



# Generating histograms and bar plots of all variables

continuous_vars <- names(summary_df)[sapply(summary_df, is.numeric)]

categorical_vars <- c(summary_df$orig_language, summary_df$countryname, summary_df$MP_parfam)

# Histograms for the numerical variables

create_hist <- function(variable) {
  p <- ggplot(summary_df, aes(x= .data[[variable]])) +
    geom_histogram() +
    theme_minimal()
  
  save_plot(paste0("plots/descriptives/", variable, "_histogram.png"), p, base_width = 6, base_height = 4)
}


# looping through the columns to create histograms for each variable

for (var in continuous_vars[3:length(continuous_vars)]) {
  create_hist(var)
}

# Bar plots for the categorical variables

create_bar <- function(variable) {
  p <- ggplot(summary_df, aes(x= .data[[variable]])) +
    geom_bar() +
    theme_minimal()
  
  save_plot(paste0("plots/descriptives/", variable, "_barplot.png"), p, base_width = 6, base_height = 4)
}

# Looping through the categorical variables to create bar plots

for (var in categorical_vars) {
  create_bar(var)
}  



