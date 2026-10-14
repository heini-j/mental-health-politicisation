library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(paletteer)
library(apaTables)
library(cowplot)
library(psych)
library(gtsummary)
library(patchwork)
library(forcats)

#' This code generates descriptive statistics and plots of all the variables. 


# Reading the clean df to R

summary_df <- read_csv("data/clean_summary.csv",
                       col_types = cols(party_id = col_character(), 
                                        month = col_number(), MP_parfam = col_character()))

View(summary_df)

# Plotting all variables ----

# Making a list of all continuous and categorical variables separately for plotting

continuous_vars <- names(summary_df)[sapply(summary_df, is.numeric)]

categorical_vars <- c("orig_language", "MP_parfam", "country")


# Creating a function to create histograms for the continuous variables

create_hist <- function(variable) {
  p <- ggplot(summary_df, aes(x= .data[[variable]])) +
    geom_histogram() +
    theme_minimal()
  
  save_plot(paste0("plots/descriptives/", variable, "_histogram.png"), p, base_width = 6, base_height = 4)
}


# looping through the columns; first 3 are date, months and manifesto id:s, which we do not want to plot 

for (var in continuous_vars[3:length(continuous_vars)]) {
  create_hist(var)
}

# Function to create bar plots for the categorical variables

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


# Summary tables -----


# Summarising the number of manifestos per year and country

document_summary <- summary_df |>
  group_by(country, year) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(year) |>
  pivot_wider(
    names_from = year,
    values_from = n,
    values_fill = 0
  ) 

View(document_summary)

# Saving the summary table as a csv file

write_excel_csv(document_summary, "data/manifestos_summary.csv")


summary_df |>
  filter(country == "Finland" & MP_parfam == "ETH") |>
  select(partyname)

#  Descriptive statistics table ----

# For all variables and by country

summary_combined <- summary_df |>
  select(country,
         orig_language, 
         translated,
         MP_parfam,
         total_rows,
         mh_log,
         ratio,
         MP_rightleft,
         lrgen,
         lrecon,
         galtan,
         GGE_health,
         MP_pervote
  ) |>
  tbl_summary(
    by = country,
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) [{min}, {max}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no",
    sort = list(all_categorical() ~ "frequency")
  ) |>
  add_overall()

parties_by_country <- summary_df |>
  group_by(country) |>
  summarise(
    n_parties = n_distinct(party_id, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = country,
    values_from = n_parties
  )

# Saving the descriptives table as a csv file

summary_combined |>
  as_tibble() |>
  bind_rows(parties_by_country) |>
  write_excel_csv("results/descriptives_table.csv")


# Summarising plots ----

# Creating a plot that shows the length of the documents and the number of mental health references over time

rows_summary <- summary_df |>
  group_by(country, year) |>
  summarise(n_rows = mean(total_rows),
            n_mh = mean(rows_classified_1),
            n_documents = n(),
            .groups = "drop")

View(rows_summary)

summary_plot <- rows_summary |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = n_rows, color = "Length of documents")) +
  geom_line(aes(y = n_mh*10, color = "Mental health references")) +
  facet_wrap(~country, scales = "free") +
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
  scale_color_manual(
    values = c(
      "Length of documents" = "#DC3B34FF",
      "Mental health references" = "#04225CFF"
    )
  ) +
  theme_cowplot() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    
    # Left axis: total quasi-sentences
    axis.title.y.left = element_text(color = "#DC3B34FF"),
    axis.text.y.left = element_text(color = "#DC3B34FF"),
    axis.ticks.y.left = element_line(color = "#DC3B34FF"),
    axis.line.y.left = element_line(color = "#DC3B34FF"),
    
    # Right axis: mental health references
    axis.title.y.right = element_text(color = "#04225CFF"),
    axis.text.y.right = element_text(color = "#04225CFF"),
    axis.ticks.y.right = element_line(color = "#04225CFF"),
    axis.line.y.right = element_line(color = "#04225CFF")
  )


save_plot("plots/document_summary.png", summary_plot, base_height = 6, base_width = 10)



# Plotting difference for parfams

summary_parties <- summary_df |>
  group_by(MP_parfam, year, country) |>
  summarise(mean_ratio = mean(mh_log, na.rm = TRUE),
            .groups = "drop")


# Line plot, party families in different colors, faceted by country
ggplot(summary_parties, aes(x = year, y = mean_ratio, color = MP_parfam)) +
  geom_line() +
  facet_wrap(~country, scales = "free") +
  theme_minimal() +
  labs(title = "Mean log of mental health references by party family and country",
       x = "Year",
       y = "Mean log of mental health references") +
  theme_cowplot()

# Boxplots of ideology variables by party family ----

# Party family x manifesto left-right ideology

# Creating a party order to arrange the boxplots by ideology, from left to right
party_order <- summary_df |>
  group_by(MP_parfam) |>
  summarise(MP_rightleft = mean(MP_rightleft, na.rm = TRUE)) |>
  arrange(desc(MP_rightleft)) |>
  pull(MP_parfam)

# Creating a new dataset with the factors
plot_data_01 <- summary_df |>
  mutate(MP_parfam = factor(MP_parfam, levels = party_order))

# Plotting it
manifestorl <- ggplot(plot_data_01, aes(x = MP_rightleft, y = MP_parfam)) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = NULL,
       x = "Manifesto right-left ideology",
       y = "Party family") +
  theme_cowplot()

save_plot("plots/MP_rightleft_by_parfam.png", manifestorl, base_height = 6, base_width = 10)

# Party family x general left-right ideology

# Ordering by the first variable, lrgen for reference for the other groups
party_order <- summary_df |>
  group_by(MP_parfam) |>
  summarise(lrgen = mean(lrgen, na.rm = TRUE)) |>
  arrange(desc(lrgen)) |>
  pull(MP_parfam)

plot_data_02 <- summary_df |>
  mutate(MP_parfam = factor(MP_parfam, levels = party_order))

plot_lrgen <- ggplot(plot_data_02, aes(x = lrgen, y = MP_parfam)) +
  geom_boxplot() +
  labs(title = NULL,
       x = "Party general left-right ideology",
       y = "Party family") +
  theme_cowplot()

plot_lrecon <- ggplot(plot_data_02, aes(x = lrecon, y = MP_parfam)) +
  geom_boxplot() +
  labs(title = NULL,
       x = "Party economic left-right ideology",
       y = "Party family") +
  theme_cowplot()

plot_galtan <- ggplot(plot_data_02, aes(x = galtan, y = MP_parfam)) +
  geom_boxplot() +
  labs(title = NULL,
       x = "Party GAL-TAN ideology",
       y = "Party family") +
  theme_cowplot()

# Combining to a single plot
party_ideology_plot <- wrap_plots(list(plot_lrgen, plot_lrecon, plot_galtan), 
                                  axes = "collect",
                                  axis_titles = "collect_y")

save_plot("plots/party_ideology_by_parfam.png", party_ideology_plot, base_height = 6, base_width = 10)


 
 # Correlation table ----
 
 # Selecting variables for a correlation table
 
 correlation_vars <- summary_df |>
   select(c("mh_log",
            "year", 
            "MP_parfam",
            "MP_rightleft", 
            "lrgen", 
            "lrecon", 
            "galtan",
            "GGE_health",
            "MP_pervote"))
 
 
 
 # Creating the table, saving as a doc file
apa.cor.table(correlation_vars,
              filename = "results/correlation_table.doc",
              show.sig.stars = TRUE,
              landscape = TRUE) 


 
 # Group comparisons----
 
party_order <- summary_df |>
  group_by(MP_parfam) |>
  summarise(mh_log = mean(mh_log, na.rm = TRUE)) |>
  arrange(desc(mh_log)) |>
  pull(MP_parfam)

plot_data_03 <- summary_df |>
  mutate(MP_parfam = factor(MP_parfam, levels = party_order))

## Maybe ridgeline plot?
ggplot(plot_data_03, aes(x = mh_log, y = MP_parfam)) +
  geom_point(alpha=0.5) +
  facet_grid(~country) +
  labs(title = "Distribution of log of mental health references by party family",
       x = "Party family",
       y = "Log of mental health references") +
  theme_cowplot()
                     
 

