# Investigate biomass and number of juvenile produced in different food treatments


# packages 
pkgs <- c("readxl", "dplyr", "fields", "plotrix", "tidyr", "ggplot2", "car",
          "tidyverse", "reshape2", "multcompView", "janitor", "lme4")
vapply(pkgs, library, FUN.VALUE = logical(1L), character.only = TRUE,
       logical.return = TRUE)

# Load dataset
rawdata <- read_excel("data/all_data_maja.xlsx")
#rawdata <- read_excel("data/all_data.xlsx", sheet = "raw_data_simplified")

# Delete the data from pilot experiment, bad observations and Hannas seaweed experiment   
yesdata <- rawdata[rawdata$`count_data`=='yes',]  

# Clean row names
yesdata <- yesdata %>%
  mutate(food = gsub("'", "", food))

yesdata <- yesdata %>%
  mutate(food = gsub(" ", "_", food))

yesdata <- yesdata %>%
  mutate(food = gsub("-", "_", food))

yesdata <- yesdata %>%
  mutate(food = gsub(":", "_", food))

# Clean column names
yesdata %>%
  clean_names() -> yesdata


#library(ggthemes)

yesdata$food <- gsub("_", " ", yesdata$food)

yesdata$food <- factor(yesdata$food,                 # Relevel group factor
                       levels = c("Baking yeast", "Oatmeal", "Oatmeal Wine Yeast Coffee Grind (33 33 33)", "Oatmeal after 3 weeks", "Oatmeal Brewers grain (50 50)", 
                                  "Oatmeal Coffee Grind (50 50)", "Wine yeast", "Oatmeal Wine Yeast (50 50)", "Oatmeal Protein powder (66 33)", "Oatmeal Oil (80 20)",
                                  "Oatmeal Protein powder (50 50)", "Fat reduced oatmeal", "Coffee Grind Protein powder (33 66)", "Yeast extract", "Coffee Grind Protein powder (66 33)",
                                  "Brewers yeast", "Brewers grain", "Protein powder", "Sugar kelp", "Coffee Grind Oil (66 33)", "Coffee Grind", "Potato starch", "No feeding"))


###############################################################################
# Add normalized biomass within each series
yesdata <- yesdata %>%
  group_by(series) %>%
  mutate(
    control_mean_biomass = mean(biomass[food == "Oatmeal"], na.rm = TRUE),  # Mean of the control in each series
    control_mean_number = mean(number[food == "Oatmeal"], na.rm = TRUE),  # Mean of the control in each series
    norm_biomass = biomass / control_mean_biomass,                          # Normalized biomass
    norm_number = number / control_mean_number                          # Normalized number
  )

# There is an outlier in wine yeast with only 5 individuals

yesdata <- yesdata %>%
  filter(!(food == "Wine yeast" & number == 5))


# Step 2: Calculate summary statistics (mean and SE) for each food and series
summary_data <- yesdata %>%
  group_by(food, series) %>%
  summarise(
    mean_norm_biomass = mean(norm_biomass, na.rm = TRUE),
    se_norm_biomass = sd(norm_biomass, na.rm = TRUE) / sqrt(n()),
    mean_norm_number = mean(norm_number, na.rm = TRUE),
    se_norm_number = sd(norm_number, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# View the summary data
print(summary_data)


###############################################################################

# Put everything together in one plot

###############################################################################

#Change treatment names because they are too long

summary_data$food <- as.character(summary_data$food)


summary_data$food[summary_data$food == "Baking yeast"] <- "Bake Yeast"
summary_data$food[summary_data$food == "Oatmeal Wine Yeast Coffee Grind (33 33 33)"] <- "Oat-WY-CG 33:33:33"
summary_data$food[summary_data$food == "Oatmeal after 3 weeks"] <- "Oatmeal 3wk"
summary_data$food[summary_data$food == "Oatmeal Brewers grain (50 50)"] <- "Oat-BG 50:50"
summary_data$food[summary_data$food == "Oatmeal Coffee Grind (50 50)"] <- "Oat-CG 50:50"
summary_data$food[summary_data$food == "Wine yeast"] <- "Wine Yeast"
summary_data$food[summary_data$food == "Oatmeal Wine Yeast (50 50)"] <- "Oat-WY 50:50"
summary_data$food[summary_data$food == "Oatmeal Protein powder (66 33)"] <- "Oat-PP 66:33"
summary_data$food[summary_data$food == "Oatmeal Oil (80 20)"] <- "Oat-Oil 80:20"
summary_data$food[summary_data$food == "Oatmeal Protein powder (50 50)"] <- "Oat-PP 50:50"
summary_data$food[summary_data$food == "Fat reduced oatmeal"] <- "Low-fat Oat"
summary_data$food[summary_data$food == "Coffee Grind Protein powder (33 66)"] <- "CG-PP 33:66"
summary_data$food[summary_data$food == "Yeast extract"] <- "Yeast Extr."
summary_data$food[summary_data$food == "Coffee Grind Protein powder (66 33)"] <- "CG-PP 66:33"
summary_data$food[summary_data$food == "Brewers yeast"] <- "Brew Yeast"
summary_data$food[summary_data$food == "Brewers grain"] <- "Brew Grain"
summary_data$food[summary_data$food == "Protein powder"] <- "Prot. Powder"
summary_data$food[summary_data$food == "Sugar kelp"] <- "Sugar Kelp"
summary_data$food[summary_data$food == "Coffee Grind Oil (66 33)"] <- "CG-Oil 66:33"
summary_data$food[summary_data$food == "Potato starch"] <- "Pot. Starch"
summary_data$food[summary_data$food == "No feeding"] <- "No Feed"


# We want graphs to be ordered according to biomass

food_order <- summary_data %>%
  group_by(food) %>%
  summarise(mean_biomass = mean(mean_norm_biomass)) %>%
  arrange(mean_biomass) %>%
  pull(food)

# Assign ordered factor levels to `food`
summary_data$food_ordered <- factor(summary_data$food, levels = food_order)


# Biomass plot
biomass <- ggplot(summary_data, aes(y = food_ordered, x = mean_norm_biomass, fill = series)) + 
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "black"
  ) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), fill = "#2A788EFF", color = "black") +
  geom_errorbar(aes(xmin = mean_norm_biomass - se_norm_biomass, xmax = mean_norm_biomass + se_norm_biomass, group = series), 
                width = 0.0, position = position_dodge(width = 0.8), color = "black") +
  geom_point(aes(group = series), position = position_dodge(width = 0.8), 
             shape = 21, size = 2.5, fill = "white", color = "black") +
  labs(title = "Normalized Biomass",
       x = "Normalized Biomass (mean ± SE)",
       y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        plot.margin = margin(6, 0, 6, 1, "cm"),
        legend.position = "none") +
  coord_cartesian(xlim = c(0.1, 1.8))  # Shrink space on the left

# Number of juveniles plot
number <- ggplot(summary_data, aes(y = food_ordered, x = mean_norm_number, fill = series)) + 
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "black"
  ) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), fill = "#7AD151FF", color = "black") +
  geom_errorbar(aes(xmin = mean_norm_number - se_norm_number, xmax = mean_norm_number + se_norm_number, group = series), 
                width = 0.0, position = position_dodge(width = 0.8), color = "black") +
  geom_point(aes(group = series), position = position_dodge(width = 0.8), 
             shape = 21, size = 2.5, fill = "white", color = "black") +
  labs(title = "Normalized Number of Juveniles",
       x = "Normalized Number (mean ± SE)",
       y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        plot.margin = margin(6, 1, 6, 0, "cm"),
        legend.position = "none") +
  coord_cartesian(xlim = c(0.1, 1.8))  # Shrink space on the left

# Combine the two plots using patchwork
require(patchwork)
combined_plot <- biomass + number + plot_layout(ncol = 2)
combined_plot

###############################################################################
# Statistics

# Make sure "oatmeal" is the reference level
yesdata$food <- relevel(yesdata$food, ref = "Oatmeal")

m.1 <- lmer(norm_biomass ~ food + (1 | series), data = yesdata)
summary(m.1)

residuals <- residuals(m.1)
hist(residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

library(multcomp)

# Apply Dunnett's test
dunnett_test <- glht(m.1, linfct = mcp(food = "Dunnett"))

# Show the results
summary(dunnett_test)

qqnorm(residuals)
qqline(residuals, col = "red")

plot(m.1)

shapiro.test(residuals)

performance::check_model(m.1)

lm_model <- lm(norm_biomass + 1 ~ food, data = yesdata)  # Shift response to be positive
boxcox(lm_model, lambda = seq(-2, 2, by = 0.1))  # Find best transformation

#There are a few issues with the residuals
# Try to log biomass

m.2 <- lmer(log(norm_biomass) ~ food + (1 | series), data = yesdata)
summary(m.2)

anova(m.2)

# Apply Dunnett's test
dunnett_test <- glht(m.2, linfct = mcp(food = "Dunnett"))

# Show the results
summary(dunnett_test)

# Extract the summary of the Dunnett test
dunnett_summary <- summary(dunnett_test)

# Create a data frame with the test results
dunnett_table <- data.frame(
  Treatment = names(dunnett_summary$test$coefficients),  # Use names() to extract the treatment names
  Estimate = dunnett_summary$test$coefficients,
  Std.Error = sqrt(diag(vcov(dunnett_test))),  # Calculate standard errors manually from the covariance matrix
  t.value = dunnett_summary$test$tstat,
  Pvalue = dunnett_summary$test$pvalues
)


# View the table
print(dunnett_table)

require(writexl)
#write_xlsx(dunnett_table, "output/Dunnett_Test_Results_Biomass.xlsx")


residuals <- residuals(m.2)
hist(residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

qqnorm(residuals)
qqline(residuals, col = "red")

plot(m.2)

shapiro.test(residuals)

library(nortest)
lillie.test(residuals)

performance::check_model(m.2)

# Try robust standard errors
robust_se <- coef_test(m.2, vcov = "CR2")  # "CR2" is a recommended choice
print(robust_se)

library(emmeans)

# Pairwise comparisons
pairwise <- emmeans(model, pairwise ~ food, adjust = "tukey")
summary(pairwise)

#The data is not sufficiently normally distributed. It helps to log() biomass
# but according to the shapiro test, it is not enough

m.3 <- lmer(sqrt(norm_biomass) ~ food + (1 | series), data = yesdata)
summary(m.3)

performance::check_model(m.3)

residuals <- residuals(m.3)
hist(residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

qqnorm(residuals)
qqline(residuals, col = "red")

plot(m.3)

shapiro.test(residuals)

library(nortest)
lillie.test(residuals)

# Squared transformation seems to be slightly better. It passes the normality test

#Number of juvenile worms

m.1 <- lmer(norm_number ~ food + (1 | series), data = yesdata)
summary(m.1)

# Apply Dunnett's test
dunnett_test <- glht(m.1, linfct = mcp(food = "Dunnett"))

# Show the results
summary(dunnett_test)

performance::check_model(m.1)

# The model diagnostics suggest a bad model fit, so we need to transform the
# reponse variable. 

################################################################################
####################### Checking which transformation is best ##################

library(MASS)

lm_model <- lm(norm_number + 1 ~ food, data = yesdata)  # Shift response to be positive
boxcox(lm_model, lambda = seq(-2, 2, by = 0.1))  # Find best transformation

# lambda is negative which suggests that the best transformation is taking the
# inverse

lm_model <- lm(1 / sqrt(norm_number) + 1 ~ food, data = yesdata)  # Shift response to be positive
boxcox(lm_model, lambda = seq(-2, 2, by = 0.1))  # Find best transformation

# But the diagnostic plots suggest that taking the inverse generates a very poor fit
# See below

###############################################################################

m.2 <- lmer(1 / norm_number ~ food + (1 | series), data = yesdata)
summary(m.2)

# Apply Dunnett's test
dunnett_test <- glht(m.2, linfct = mcp(food = "Dunnett"))

# Show the results
summary(dunnett_test)

performance::check_model(m.2)

# Inverse is bad

# Try squared or log

m.3 <- lmer(log(norm_number) ~ food + (1 | series), data = yesdata)
summary(m.3)

performance::check_model(m.3)

m.4 <- lmer(sqrt(norm_number) ~ food + (1 | series), data = yesdata)
summary(m.4)

performance::check_model(m.4)

residuals <- residuals(m.4)
hist(residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

qqnorm(residuals)
qqline(residuals, col = "red")

plot(m.4)

shapiro.test(residuals)

# Log and square root are very similar. It seems that there are problems with
# heteroschedascity for both models. Try glm with log and inverse family?


m.5 <- glmer(norm_number ~ food + (1 | series), family = Gamma(link = "inverse"), data = yesdata)
summary(m.5)

# Apply Dunnett's test
dunnett_test <- glht(m.5, linfct = mcp(food = "Dunnett"))

# Show the results
summary(dunnett_test)

library(DHARMa)

performance::check_model(m.5)

m.6 <- glmer(norm_number ~ food + (1 | series), family = Gamma(link = "log"), data = yesdata)
summary(m.6)

performance::check_model(m.6)

# Similar issues as with lm

###############################################################################

# Create a data frame with the test results
dunnett_table <- data.frame(
  Treatment = names(dunnett_summary$test$coefficients),  # Use names() to extract the treatment names
  Estimate = dunnett_summary$test$coefficients,
  Std.Error = sqrt(diag(vcov(dunnett_test))),  # Calculate standard errors manually from the covariance matrix
  t.value = dunnett_summary$test$tstat,
  Pvalue = dunnett_summary$test$pvalues
)

performance::check_model(m.2)

m.2 <- lmer(log(norm_number) ~ food + (1 | series), data = yesdata)
summary(m.2)

residuals <- residuals(m.2)
hist(residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

qqnorm(residuals)
qqline(residuals, col = "red")

plot(m.2)

shapiro.test(residuals)


performance::check_model(m.2)

#Save output
#write_xlsx(dunnett_table, "output/Dunnett_Test_Results_Number.xlsx")





