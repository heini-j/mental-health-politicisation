library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(paletteer)
library(car)
library(cowplot)
library(gtsummary)

#' This code generates descriptive statistics and plots of all the variables. 


# Reading the clean df to R

summary_df <- read_csv("data/clean_summary.csv",
                       col_types = cols(party_id = col_character(), 
                                        month = col_number(), MP_parfam = col_character()))

View(summary_df)


# Plotting all variables ----

# Making a list of all continuous and categorical variables separately for plotting

continuous_vars <- names(summary_df)[sapply(summary_df, is.numeric)]

categorical_vars <- c("orig_language", "MP_parfam", "countryname")

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
  group_by(countryname, year) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(year) |>
  pivot_wider(
    names_from = countryname,
    values_from = n,
    values_fill = 0
  ) 

View(document_summary)

# Saving the summary table as a csv file

write_excel_csv(document_summary, "data/manifestos_summary.csv")

# Descriptives table 

summary_table <- summary_df|>
  select(-c(manifesto_id, party_id, date, partyname, year, month)) |>
  tbl_summary(
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no",
    sort = list(all_categorical() ~ "frequency")
  )


# Saving the descriptives table as a csv file

summary_table |>
  as_tibble() |>
  write_excel_csv("data/descriptives_table.csv")


# Summarising plots ----

# Creating a plot that shows the length of the documents and the number of mental health references over time

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


# Same for ratio variable

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
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")
 

 
 # Correlation table ----
 
 # Selecting variables for a correlation table
 
 correlation_vars <- summary_df |>
   select(c("ratio",
            "year", 
            "MP_rightleft", 
            "lrgen", 
            "lrecon", 
            "galtan", 
            "redistribution", 
            "spendvtax",
            "MP_human_rights", 
            "MP_equality", 
            "MP_welfare_exp", 
            "MP_labourgroups", 
            "MP_pervote",
            "MP_totseats",
            "total_rows"))
 
 
   
 
 # Creating the table, saving as a doc file
 apa.cor.table(correlation_vars,
               filename = "data/correlation_table.doc",
               show.sig.stars = TRUE,
               landscape = TRUE)

 
 # Group comparisons----
 
 
 parfam_means <- summary_df |>
   group_by(MP_parfam) |>
   summarise(ratio_mean = mean(ratio, na.rm = TRUE),
             ratio_sd = sd(ratio, na.rm = TRUE),
             n = n(),
             .groups = "drop")

                     
 # making a boxplot to see if there are differences in the ratio variable between the party families
 
 summary_df |> filter(party_id == "13230") |> select("partyname")
 
 summary_df |> filter(between(year, 1999, 2002)) |>
   ggplot(aes(y=ratio)) +
   geom_boxplot() +
   facet_wrap(~MP_parfam) +
   theme_minimal()
 
 
 ?geom_boxplot
 
 
 ggplot(summary_df, aes(x = MP_parfam, y = ratio)) +
   geom_violin() +
   theme_minimal()
 
 ggplot(summary_df, aes(x = countryname, y = ratio)) +
   geom_violin() +
   theme_minimal()
 
 
 ggplot(summary_df, aes(x = MP_rightleft, y = ratio)) +
   geom_point(alpha = 0.5)+
   theme_minimal()
 
 ggplot(summary_df, aes(x = lrgen, y = ratio)) +
   geom_point(alpha = 0.5)+
   theme_minimal()
 
 ggplot(summary_df, aes(x = lrecon, y = ratio)) +
   geom_point(alpha = 0.5)+
   theme_minimal()
 
 ggplot(summary_df, aes(x = galtan, y = ratio)) +
   geom_point(alpha = 0.5)+
   theme_minimal()
 
 ggplot(summary_df, aes(x = spendvtax, y = ratio)) +
   geom_point(alpha = 0.5)+
   theme_minimal()
 
 ggplot(summary_df, aes(x = redistribution, y = ratio)) +
   geom_point(alpha = 0.5)+
   theme_minimal()
 
 
 ggplot(summary_df, aes(x = MP_human_rights, y = ratio)) +
   geom_point(alpha = 0.5)+
   geom_smooth(method = "lm")+
   theme_minimal()
 
 summary_df |>
   filter(year>1960) |>
   ggplot(aes(x = year, y = ratio)) +
   geom_point(alpha = 0.5)+
   geom_smooth(method = "loess")+
   facet_wrap(~countryname, scales = "free")+
   theme_cowplot()
 
ratio_yearly <- summary_df |>
  filter(year>= 1960) |>
  group_by(year) |>
  summarise(mean_ratio = mean(ratio, na.rm = TRUE),
            sd_ratio = sd(ratio, na.rm = TRUE))
 
 ggplot(ratio_yearly, (aes(x = year, y = mean_ratio))) +
          geom_line() +
          theme_minimal() 
 
 ratio_yearly_country <- summary_df |>
   filter(year>= 1960) |>
   group_by(year, countryname, MP_parfam) |>
   summarise(mean_ratio = mean(ratio, na.rm = TRUE),
             sd_ratio = sd(ratio, na.rm = TRUE))


 ggplot(ratio_yearly_country, (aes(x = year, y = mean_ratio, color = MP_parfam))) +
   geom_line() +
   facet_wrap(~countryname, scales = "free") +
   theme_minimal() 


