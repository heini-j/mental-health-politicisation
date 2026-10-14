library(readr)
library(apaTables)

model_data <- read_csv("data/clean_summary.csv")

# Anova for the party families

anova_data <- model_data |>
  filter(year >= 1999) |> # too few parties before
  filter(MP_parfam %in% c("SOSDEM", "CON", "NAT", "LEFT", "CHR")) # 5 largest groups

# changing the party family as factor
anova_data$MP_parfam <- as.factor(anova_data$MP_parfam)


# Levene test for homogeneity of variance to see if method is appropriate
leveneTest(ratio_log ~ MP_parfam, data = anova_data, center = mean)

# ANOVA test
anova <- aov(ratio_log ~ MP_parfam, data = anova_data)

results <- summary(anova)

# Saving the means and standard deviations
apa.1way.table(iv = MP_parfam, dv = ratio_log, data = anova_data, filename = "results/anova_table.doc")

# Post hoc test with Tukey HDS to see the pairwise comparisons
TukeyHSD(anova)


