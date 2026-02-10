############################################################
# ENS 495 / Environmental Statistics
# Exploratory Data Analysis (EDA) & Graphing in R
# Focus: Water Quality & Estuarine Ecology (Florida Estuaries)
#
# This R script is meant to be READ top-to-bottom.
# All explanations are written as comments (lines starting with #).
# You should run code in small chunks and inspect the output as you go.
############################################################

############################################################
# 1. WHY EXPLORATORY DATA ANALYSIS?
############################################################
# Exploratory Data Analysis (EDA) is the first step in any statistical analysis.
# In environmental and marine biology, EDA helps us:
#  - Understand the typical conditions of an ecosystem
#  - Detect extreme values (e.g., heat waves, hypoxia events)
#  - Evaluate assumptions for statistical tests
#  - Compare variability across sites or habitats
#
# Before running models or hypothesis tests, we ALWAYS explore the data.
############################################################

############################################################
# 2. PACKAGES
############################################################
# ggplot2: modern plotting system
# dplyr: data manipulation
# moments: skewness and kurtosis
# car: tests for equality of variances

install.packages(c("ggplot2", "dplyr", "moments", "car"))

library(ggplot2)
library(dplyr)
library(moments)
library(car)

############################################################
# 3. DATASET: FLORIDA ESTUARY WATER QUALITY
############################################################
# We will use ONE dataset throughout the script.
# This mimics a real monitoring program in Florida estuaries.

set.seed(230)  # ensures reproducibility

estuaries <- data.frame(
  Estuary = rep(c("Indian River Lagoon", "Tampa Bay", "Apalachicola Bay"), each = 60),
  Temperature_C = c(
    rnorm(60, mean = 27, sd = 2),
    rnorm(60, mean = 26, sd = 2.5),
    rnorm(60, mean = 25, sd = 1.8)
  ),
  Dissolved_Oxygen_mgL = c(
    rnorm(60, mean = 6.5, sd = 1.2),
    rnorm(60, mean = 7.2, sd = 1.0),
    rnorm(60, mean = 7.8, sd = 0.8)
  ),
  Salinity_psu = c(
    rnorm(60, mean = 32, sd = 3),
    rnorm(60, mean = 28, sd = 4),
    rnorm(60, mean = 20, sd = 5)
  )
)

# Always inspect your data
str(estuaries)
summary(estuaries)
head(estuaries)
#Change Estuary to a factor
estuaries$Estuary<-as.factor(estuaries$Estuary)
class(estuaries$Estuary)

############################################################
# 4. MEASURES OF LOCATION
############################################################
# Measures of location describe the CENTER of the data.
# In environmental science, this tells us what conditions are typical.

mean(estuaries$Temperature_C)
median(estuaries$Temperature_C)

# Mean: sensitive to extreme values (heat waves)
# Median: more robust when distributions are skewed

############################################################
# 5. MEASURES OF SPREAD
############################################################
# Spread tells us how variable the environment is.
# High variability often means higher physiological stress for organisms.

sd(estuaries$Temperature_C)
var(estuaries$Temperature_C)

# Skewness tells us about asymmetry
skewness(estuaries$Temperature_C)

# Positive skew: occasional extreme high temperatures
# Negative skew: occasional extreme low temperatures


############################################################
# 6. SUMMARY STATISTICS BY ESTUARY
############################################################
# We often want summaries by groups (e.g., different estuaries)

summary_by_estuary <- estuaries %>%
  group_by(Estuary) %>%
  summarise(
    mean_temp = mean(Temperature_C),
    sd_temp = sd(Temperature_C),
    var_temp = var(Temperature_C),
    skew_temp = skewness(Temperature_C)
  )

summary_by_estuary

############################################################
# 7. VISUALIZING DISTRIBUTIONS (UNIVARIATE)
############################################################
# Graphs help us understand shape, spread, and outliers

# Histogram

ggplot(estuaries, aes(x = Temperature_C)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Water Temperature",
    x = "Temperature (°C)",
    y = "Frequency"
  )

# Density plot

ggplot(estuaries, aes(x = Temperature_C)) +
  geom_density(fill = "lightgreen", alpha = 0.6) +
  labs(
    title = "Density Plot of Water Temperature",
    x = "Temperature (°C)",
    y = "Density"
  )

############################################################
# 9. BOXPLOTS AND QUANTILES
############################################################
# Boxplots visualize medians, quartiles, and outliers

# Overall boxplot

ggplot(estuaries, aes(y = Temperature_C)) +
  geom_boxplot(fill = "tan") +
  labs(
    title = "Boxplot of Water Temperature",
    y = "Temperature (°C)"
  )

# Boxplot by estuary

ggplot(estuaries, aes(x = Estuary, y = Temperature_C, fill = Estuary)) +
  geom_boxplot() +
  labs(
    title = "Water Temperature by Florida Estuary",
    x = "Estuary",
    y = "Temperature (°C)"
  )

############################################################
# 9. SIDE-BY-SIDE COMPARISON: DENSITY PLOT vs BOXPLOT
############################################################
# Different plots highlight different features of the same data.
# Comparing them side-by-side helps us understand:
#   - Distribution shape (density plot)
#   - Median, spread, and outliers (boxplot)
#
# We are using Temperature as an example, but this approach
# works for any continuous variable.

############################################################
# CREATE THE DENSITY PLOT
############################################################

density_plot <- ggplot(estuaries, aes(x = Temperature_C)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  labs(
    title = "Density Plot of Water Temperature",
    x = "Temperature (°C)",
    y = "Density"
  )

############################################################
#CREATE THE BOXPLOT
############################################################

boxplot_plot <- ggplot(estuaries, aes(y = Temperature_C)) +
  geom_boxplot(fill = "tan") +
  labs(
    title = "Boxplot of Water Temperature",
    y = "Temperature (°C)"
  )

############################################################
# DISPLAY PLOTS SIDE BY SIDE
############################################################
# The gridExtra package allows us to arrange multiple plots
# in a single viewing window.

install.packages("gridExtra")
library(gridExtra)

grid.arrange(density_plot, boxplot_plot, ncol = 2)

############################################################
# HOW TO INTERPRET THIS COMPARISON
############################################################
# Use the density plot to assess:
#   - Symmetry vs skewness
#   - Number of peaks (unimodal vs multimodal)
#
# Use the boxplot to assess:
#   - Median temperature
#   - Interquartile range (IQR)
#   - Presence of outliers
#
# Together, these plots provide a more complete picture
# of the distribution than either plot alone.
############################################################


############################################################
# 10. BIVARIATE PLOTS & GRAPHICAL INDEPENDENCE
############################################################
# Scatterplots help us evaluate relationships between variables
# Graphical independence: if no clear pattern exists, variables may be independent

# Temperature vs Dissolved Oxygen

ggplot(estuaries, aes(x = Temperature_C, y = Dissolved_Oxygen_mgL)) +
  geom_point() +
  labs(
    title = "Temperature vs Dissolved Oxygen",
    x = "Temperature (°C)",
    y = "Dissolved Oxygen (mg/L)"
  )

# In estuaries, DO often decreases as temperature increases

############################################################
# 11. QQ PLOTS (NORMALITY CHECK)
############################################################
# QQ plots compare the data to a theoretical normal distribution
# Points close to the line indicate approximate normality

qqnorm(estuaries$Temperature_C)
qqline(estuaries$Temperature_C)

# Deviations at the tails suggest skewness or extreme events

############################################################
# 12. TESTING DISTRIBUTIONAL ASSUMPTIONS
############################################################
# Many statistical tests assume:
# 1. Normality
# 2. Equal variances among groups

############################################################
# 13. EQUALITY OF VARIANCES
############################################################
# Levene's Test evaluates whether variances are equal across groups

leveneTest(Temperature_C ~ Estuary, data = estuaries)

# If p > 0.01: variances are approximately equal
# If p < 0.01: variances differ (heteroscedasticity)

############################################################
# 14. ADDITIONAL EXPLORATORY ANALYSIS
############################################################
# EDA is iterative. We often explore the SAME variables in multiple ways
# to reveal patterns we might otherwise miss.

############################################################
# 14A. FACETED DISTRIBUTIONS
############################################################
# Faceting allows us to compare distributions across estuaries directly

# Temperature distributions by estuary

ggplot(estuaries, aes(x = Temperature_C)) +
  geom_histogram(bins = 15, fill = "lightblue", color = "black") +
  facet_wrap(~Estuary) +
  labs(
    title = "Temperature Distributions by Estuary",
    x = "Temperature (°C)",
    y = "Frequency"
  )

############################################################
# 14B. BOXPLOTS FOR MULTIPLE VARIABLES
############################################################
# Boxplots are useful for comparing spread and medians across groups

# Dissolved oxygen by estuary

ggplot(estuaries, aes(x = Estuary, y = Dissolved_Oxygen_mgL, fill = Estuary)) +
  geom_boxplot() +
  labs(
    title = "Dissolved Oxygen by Estuary",
    x = "Estuary",
    y = "Dissolved Oxygen (mg/L)"
  )

############################################################
# 14C. CORRELATION EXPLORATION
############################################################
# Correlation measures the strength and direction of linear relationships
# between two continuous variables

cor(estuaries$Temperature_C, estuaries$Dissolved_Oxygen_mgL)

# Negative correlation values suggest that as temperature increases,
# dissolved oxygen decreases — a common ecological pattern

############################################################
# 14D. PAIRWISE SCATTERPLOTS (GRAPHICAL EXPLORATION)
############################################################
# Pairwise plots allow us to examine multiple bivariate relationships at once

pairs(estuaries[, c("Temperature_C", "Dissolved_Oxygen_mgL", "Salinity_psu")])

############################################################
# 15. WHY THIS MATTERS ECOLOGICALLY
############################################################
# Understanding distributions, relationships, and variability helps us:
#  - Detect environmental stressors (heat, hypoxia, freshwater input)
#  - Evaluate ecosystem stability
#  - Decide which statistical tests are appropriate
#  - Avoid incorrect assumptions before modeling
#
# EDA is not optional — it is foundational to environmental data analysis.

############################################################
# 16. PRACTICE QUESTIONS
############################################################
# Answer the following questions using BOTH graphs and summary statistics.

# Question 1:
# Which estuary appears to have the greatest variability in salinity?
# Support your answer using:
#   - A boxplot
#   - At least one numerical measure of spread
#   - A brief ecological explanation

# Question 2:
# Based on the scatterplot and correlation between temperature and dissolved oxygen:
#   a) Describe the relationship (direction and strength)
#   b) Does the relationship appear linear?
#   c) Why is this relationship important for estuarine organisms?

############################################################
# END OF SCRIPT
############################################################
