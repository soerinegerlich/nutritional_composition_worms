# Script on nutritional content in worms (Figure 3 in manuscript)

# packages
pkgs <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "tidyverse",
  "reshape2",
  "janitor"
)

vapply(
  pkgs,
  library,
  FUN.VALUE = logical(1L),
  character.only = TRUE,
  logical.return = TRUE
)

# Load dataset
rawdata <- read_excel("data/all_data_maja.xlsx")
#rawdata <- read_excel("data/all_data.xlsx", sheet = "raw_data_simplified")

# Delete the data from pilot experiment, bad observations and Hannas seaweed experiment
yesdata <- rawdata[rawdata$`count_data` == 'yes', ]

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

yesdata$food <- gsub("_", " ", yesdata$food)

yesdata$food <- factor(yesdata$food,                 # Relevel group factor
                       levels = c("Baking yeast", "Oatmeal", "Oatmeal Wine Yeast Coffee Grind (33 33 33)", "Oatmeal after 3 weeks", "Oatmeal Brewers grain (50 50)", 
                                  "Oatmeal Coffee Grind (50 50)", "Wine yeast", "Oatmeal Wine Yeast (50 50)", "Oatmeal Protein powder (66 33)", "Oatmeal Oil (80 20)",
                                  "Oatmeal Protein powder (50 50)", "Fat reduced oatmeal", "Coffee Grind Protein powder (33 66)", "Yeast extract", "Coffee Grind Protein powder (66 33)",
                                  "Brewers yeast", "Brewers grain", "Protein powder", "Sugar kelp", "Coffee Grind Oil (66 33)", "Coffee Grind", "Potato starch", "No feeding"))


# For clarification: biomass (mg) and total Fa (mg/g dw)? To calculate total amount of FA
# we calculate biomass * mean 'Total FA'/1000 = total FA mg)

yesdata$amount_fa    <-  yesdata$biomass * yesdata$total_fa / 1000 # Generate Total FA amount form biomass

###############################################################################
###################### STATISTICS #############################################
###############################################################################

yesdata$amount_protein <- (yesdata$protein_percent_dw * yesdata$biomass) /
  100 #Divided by 100 because protein is percent
yesdata$amount_ash <- (yesdata$ash_percent_dw * yesdata$biomass) /
  100 #Divided by 100 because ash is percent


df_nutrients_stats <- yesdata %>%
  dplyr::select(
    food,
    series,
    biomass,
    amount_fa,
    amount_protein,
    amount_ash
  )

df_nutrients_stats <- df_nutrients_stats %>% drop_na()

df_nutrients_stats <- df_nutrients_stats %>%
  group_by(food) %>%
  mutate(
    total_nutrient = amount_fa + amount_protein + amount_ash,
    amount_carbo = biomass-total_nutrient
  )

df_nutrients_stats <- df_nutrients_stats %>%
  group_by(food) %>%
  mutate(
    pct_protein = (amount_protein/biomass) * 100,
    pct_ash = (amount_ash/biomass) * 100,
    pct_fa = (amount_fa/biomass) * 100,
    pct_carbo = (amount_carbo/biomass) * 100
  )


# Add a replicates column
df_nutrients_stats <- df_nutrients_stats %>%
  group_by(food) %>%
  mutate(replicates = row_number())

df_nutrients_stats$replicates = as.character(df_nutrients_stats$replicates)

# Only values from series A, so no need to include random effects
# Are we mostly interested in knowing the difference from oatmeal as control?

df_nutrients_stats$pct_fa_scale = as.numeric(scale(df_nutrients_stats$pct_fa))

# Fatty acids
m.fa <- lmer(pct_fa_scale ~ food + (1 | replicates), df_nutrients_stats)
m.fa <- lmer(pct_fa ~ food + (1 | replicates), df_nutrients_stats)

summary(m.fa)

anova(m.fa)

performance::check_model(m.fa)

library(multcomp)

# Apply Dunnett's test
dunnett_test <- glht(m.fa, linfct = mcp(food = "Dunnett"))

# Show the results
summary(dunnett_test)

library(emmeans)


# Pairwise comparisons
pairwise <- emmeans(m.fa, pairwise ~ food, adjust = "tukey")
pairwise_summary <- summary(pairwise)

# Create a data frame with the test results
pairwise_table <- data.frame(
  Treatment = pairwise_summary$contrasts$contrast,  # Extract treatment comparison names
  Estimate = pairwise_summary$contrasts$estimate,
  Std.Error = pairwise_summary$contrasts$SE,
  t.value = pairwise_summary$contrasts$t.ratio,
  Pvalue = pairwise_summary$contrasts$p.value
)

pairwise_table

require(writexl)

#write_xlsx(pairwise_table, "output/Pairwise_Test_Results_Fa.xlsx")

df_nutrients_stats$pct_protein_scale = as.numeric(scale(df_nutrients_stats$pct_protein))

# Protein
m.protein <- lmer(pct_protein ~ food + (1 | replicates), df_nutrients_stats)
m.protein <- lmer(pct_protein_scale ~ food + (1 | replicates), df_nutrients_stats)

summary(m.protein)

performance::check_model(m.protein)

# Apply Dunnett's test
dunnett_test <- glht(m.protein, linfct = mcp(food = "Dunnett"))

# Show the results
summary(dunnett_test)

# Pairwise comparisons
pairwise <- emmeans(m.protein, pairwise ~ food, adjust = "tukey")
pairwise_summary <- summary(pairwise)

# Create a data frame with the test results
pairwise_table <- data.frame(
  Treatment = pairwise_summary$contrasts$contrast,  # Extract treatment comparison names
  Estimate = pairwise_summary$contrasts$estimate,
  Std.Error = pairwise_summary$contrasts$SE,
  t.value = pairwise_summary$contrasts$t.ratio,
  Pvalue = pairwise_summary$contrasts$p.value
)

pairwise_table

#write_xlsx(pairwise_table, "output/Pairwise_Test_Results_Protein.xlsx")

df_nutrients_stats$pct_ash_scale = as.numeric(scale(df_nutrients_stats$pct_ash))

# ash
m.ash <- lmer(sqrt(pct_ash) ~ food + (1 | replicates), df_nutrients_stats)
m.ash <- lmer(pct_ash_scale ~ food + (1 | replicates), df_nutrients_stats)

summary(m.ash)

performance::check_model(m.ash)


anova(m.ash)

# Apply Dunnett's test
dunnett_test <- glht(m.ash, linfct = mcp(food = "Dunnett"))

# Show the results
summary(dunnett_test)

# Pairwise comparisons
pairwise <- emmeans(m.ash, pairwise ~ food, adjust = "tukey")
pairwise_summary <- summary(pairwise)

# Create a data frame with the test results
pairwise_table <- data.frame(
  Treatment = pairwise_summary$contrasts$contrast,  # Extract treatment comparison names
  Estimate = pairwise_summary$contrasts$estimate,
  Std.Error = pairwise_summary$contrasts$SE,
  t.value = pairwise_summary$contrasts$t.ratio,
  Pvalue = pairwise_summary$contrasts$p.value
)

pairwise_table

#write_xlsx(pairwise_table, "output/Pairwise_Test_Results_Ash.xlsx")

summary(emmeans(m.fa, ~ food))


################################################################################

################################################################################
####################### PLOT ###################################################
################################################################################


# Create summary data, remove NA observations
SumTab1 <- yesdata %>%
  group_by(food, series) %>%
  summarize(
    mean_biomass = mean(biomass, na.rm = TRUE),
    se_biomass = sd(biomass, na.rm = TRUE) / sqrt(n()),
    n_biomass = n_distinct(biomass, na.rm = TRUE),
    # Remove missing values
    mean_number = mean(number, na.rm = TRUE),
    sd_number = sd(number, na.rm = TRUE),
    n_number = n_distinct(number, na.rm = TRUE),
    mean_protein_percent_dw = mean(protein_percent_dw, na.rm = TRUE),
    se_protein_percent_dw = sd(protein_percent_dw, na.rm = TRUE) / sqrt(n()),
    n_protein_percent_dw = n_distinct(protein_percent_dw, na.rm = TRUE),
    mean_ash_percent_dw = mean(ash_percent_dw, na.rm = TRUE),
    se_ash_percent_dw = sd(ash_percent_dw, na.rm = TRUE) / sqrt(n()),
    n_ash_percent_dw = n_distinct(ash_percent_dw, na.rm = TRUE),
    mean_total_fa = mean(total_fa, na.rm = TRUE),
    sd_total_fa = sd(total_fa, na.rm = TRUE),
    n_total_fa = n_distinct(total_fa, na.rm = TRUE),
    mean_total_o3_fa = mean(total_3_fa, na.rm = TRUE),
    sd_total_o3_fa = sd(total_3_fa, na.rm = TRUE),
    n_total_o3_fa = n_distinct(total_3_fa, na.rm = TRUE),
    mean_c18_3_3 = mean(c18_3_3, na.rm = TRUE),
    sd_c18_3_3 = sd(c18_3_3, na.rm = TRUE),
    n_c18_3_3 = n_distinct(c18_3_3, na.rm = TRUE),
    mean_c20_3_11_14_17_3 = mean(c20_3_11_14_17_3, na.rm = TRUE),
    sd_c20_3_11_14_17_3 = sd(c20_3_11_14_17_3, na.rm = TRUE),
    n_c20_3_11_14_17_3 = n_distinct(c20_3_11_14_17_3, na.rm = TRUE),
    mean_c20_5_3 = mean(c20_5_3, na.rm = TRUE),
    sd_c20_5_3 = sd(c20_5_3, na.rm = TRUE),
    n_c20_5_3 = n_distinct(c20_5_3, na.rm = TRUE),
    mean_c22_5_3 = mean(c22_5_3, na.rm = TRUE),
    sd_c22_5_3 = sd(c22_5_3, na.rm = TRUE),
    n_c22_5_3 = n_distinct(c22_5_3, na.rm = TRUE),
    mean_c22_6_3 = mean(c22_6_3, na.rm = TRUE),
    sd_c22_6_3 = sd(c22_6_3, na.rm = TRUE),
    n_c22_6_3 = n_distinct(c22_6_3, na.rm = TRUE),
    pctProtein = mean(pct_protein),
    #Adding the x, y and z variables for the heatmap
    pctLipid = mean(pct_lipid, na.rm = TRUE),
    pctCarbohydrate = mean(pct_carbohydrate, na.rm = TRUE),
    mean_amount_fa = mean(amount_fa ,na.rm = TRUE),
    se_amount_fa = sd(amount_fa, na.rm = TRUE) / sqrt(n())
  )


#AuxData <- merge(yesdata,MeanFAData,by="food")
AuxData <- SumTab1

AuxData$biomass_per_worm <- AuxData$mean_biomass / AuxData$mean_number
AuxData$amount_protein <- (AuxData$mean_protein_percent_dw * AuxData$mean_biomass) /
  100 #Divided by 100 because protein is percent
AuxData$amount_protein_se <- (AuxData$se_protein_percent_dw * AuxData$mean_biomass) /
  100 #Divided by 100 because protein is percent

AuxData$amount_ash <- (AuxData$mean_ash_percent_dw * AuxData$mean_biomass) /
  100 #Divided by 100 because ash is percent
AuxData$amount_ash_se <- (AuxData$se_ash_percent_dw * AuxData$mean_biomass) /
  100 #Divided by 100 because ash is percent


PlotData <- AuxData %>%
  dplyr::select(
    food,
    mean_biomass,
    se_biomass,
    mean_amount_fa,
    se_amount_fa,
    amount_protein,
    amount_protein_se,
    amount_ash,
    amount_ash_se
  )

PlotData <- PlotData %>% drop_na()

#Carbohydrate in %

df_macro <- PlotData %>%
  group_by(food) %>%
  mutate(
    total_nutrient = mean_amount_fa + amount_protein + amount_ash,
    mean_amount_carbo = mean_biomass-total_nutrient
  )

df_macro <- df_macro %>%
  group_by(food) %>%
  mutate(
    pct_protein = (amount_protein/mean_biomass) * 100,
    pct_protein_se = (amount_protein_se/mean_biomass) * 100,
    pct_ash = (amount_ash/mean_biomass) * 100,
    pct_ash_se = (amount_ash_se/mean_biomass) * 100,
    pct_fa = (mean_amount_fa/mean_biomass) * 100,
    pct_fa_se = (se_amount_fa/mean_biomass) * 100,
    pct_carbo = (mean_amount_carbo/mean_biomass) * 100
  )

#write_xlsx(df_macro, "output/worms_macronutrients.xlsx")


# Reshape data to long format
nutrient_data_long <- df_macro %>%
  pivot_longer(cols = c(pct_ash, pct_protein, pct_fa, pct_carbo), 
               names_to = "component", 
               values_to = "percentage")

#With SE values added

# Standard errors in long format
se_data_long <- df_macro %>%
  pivot_longer(cols = c(pct_ash_se, pct_protein_se, pct_fa_se), 
               names_to = "component_se", 
               values_to = "percentage_se")

#se_data_long <- se_data_long %>%
# mutate(component = factor(component, levels = c("pct_ash", "pct_carbo", "pct_protein", "pct_fa")))

se_data_long <- se_data_long %>%
  dplyr::select(
    food,
    component_se,
    percentage_se
  )

se_data_long <- se_data_long %>%
  rename(component = component_se) %>%  # Rename column first
  mutate(
    component = case_when(
      component == "pct_ash_se" ~ "pct_ash",
      component == "pct_protein_se" ~ "pct_protein",
      component == "pct_fa_se" ~ "pct_fa",
      TRUE ~ component
    )
  )

# Join the nutrient data with standard error data
df_final <- nutrient_data_long %>%
  left_join(se_data_long, by = c("food", "component"))

df_final$food <- as.character(df_final$food)

df_final$food[df_final$food == "Coffee_Grind_Oil_(66_33)"] <- "Coffee:Oil (66:33)"
df_final$food[df_final$food == "Oatmeal_Oil_(80_20)"] <- "Oatmeal:Oil (80:20)"
df_final$food[df_final$food == "Oatmeal_Protein_powder_(66_33)"] <- "Oatmeal:Protein (66:33)"
df_final$food[df_final$food == "Coffee_Grind_Protein_powder_(66_33)"] <- "Coffee:Protein (66:33)"
df_final$food[df_final$food == "Oatmeal_Protein_powder_(50_50)"] <- "Oatmeal:Protein (50:50)"
df_final$food[df_final$food == "Coffee_Grind_Protein_powder_(33_66)"] <- "Coffee:Protein (33:66)"
df_final$food[df_final$food == "Wine_yeast"] <- "Wine yeast"
df_final$food[df_final$food == "Sugar_kelp"] <- "Sugar kelp"
df_final$food[df_final$food == "Potato_starch"] <- "Potato starch"


df_final <- df_final %>%
  dplyr::select(
    food,
    component,
    percentage,
    percentage_se
  )

# Replace NA in percentage_se with 0
df_final$percentage_se[is.na(df_final$percentage_se)] <- 0


filtered_fa <- df_final %>%
  filter(component == "pct_fa")

filtered_protein <- df_final %>%
  filter(component == "pct_fa" | component == "pct_protein")

filtered_protein <- filtered_protein %>%
  arrange(food, component) %>%
  group_by(food) %>%
  mutate(cum_percentage = cumsum(percentage)) %>%
  ungroup()

filtered_protein <- filtered_protein %>%
  filter(component == "pct_protein")

filtered_ash <- df_final %>%
  filter(component == "pct_fa" | component == "pct_protein" | component == "pct_ash")

# Ensure correct order for components (optional)
filtered_ash <- filtered_ash %>%
  mutate(component = factor(component, levels = c("pct_fa", "pct_protein", "pct_ash")))

filtered_ash <- filtered_ash %>%
  arrange(food, component) %>%
  group_by(food) %>%
  mutate(cum_percentage = cumsum(percentage)) %>%
  ungroup()

# Ensure correct order for components (optional)
df_final <- df_final %>%
  mutate(component = factor(component, levels = c("pct_carbo", "pct_ash", "pct_protein", "pct_fa")))

# Step 2: Reorder 'food' by fatty acid percentage
fat_order <- df_final %>%
  filter(component == "pct_fa") %>%
  arrange(desc(percentage)) %>%
  pull(food)

df_final <- df_final %>%
  mutate(food = factor(food, levels = fat_order))



#df_final <- df_final %>%
#  arrange(food, component) %>%
#  group_by(food) %>%
#  mutate(cum_percentage = cumsum(percentage),
#         midpoint = cum_percentage - (percentage / 2)) %>%  # Midpoint for error bars
#  ungroup()

# Plot the stacked bar plot with correctly positioned error bars
ggplot() +
  geom_bar(data = df_final, 
           aes(x = food, y = percentage, fill = component), 
           stat = "identity") +
  
  # Adding error bars centered within each segment
  geom_errorbar(data = filtered_fa, 
                aes(x = food, ymin = percentage - percentage_se, ymax = percentage + percentage_se),
                width = 0.0, color = "black", linewidth = 0.8) +
  
  # Optional: Overlaying points to show the midpoints for better visualization
  geom_point(data = filtered_fa, 
             aes(x = food, y = percentage), 
             shape = 21, size = 2.5, fill = "white", color = "black") +
  
  geom_errorbar(data = filtered_protein, 
                aes(x = food, ymin = cum_percentage - percentage_se, ymax = cum_percentage + percentage_se),
                width = 0.0, color = "black", linewidth = 0.8) +
  
  geom_point(data = filtered_protein, 
             aes(x = food, y = cum_percentage), 
             shape = 21, size = 2.5, fill = "white", color = "black") +
  
  geom_errorbar(data = filtered_ash, 
                aes(x = food, ymin = cum_percentage - percentage_se, ymax = cum_percentage + percentage_se),
                width = 0.0, color = "black", linewidth = 0.8) +
  
  geom_point(data = filtered_ash, 
             aes(x = food, y = cum_percentage), 
             shape = 21, size = 2.5, fill = "white", color = "black") +
  
  scale_fill_viridis_d(option = "D",
                       name = "Nutrient component",
                       labels = c("Carbohydrate", "Ash", "Protein", "Fatty Acid")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 101)) +
  
  labs(
    title = "Composition of Worms by Treatment",
    x = "",
    y = "% of worm DM",
    fill = "Component"
  ) +
  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
        plot.margin = margin(6, 2, 6, 2, "cm"))






