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
# Plot with cleaned labels
ggplot(yesdata, aes(biomass, food)) + 
  geom_boxplot(aes(fill = factor(series)),
               linewidth = 0.3, 
               width = 0.6) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6)) + 
  scale_fill_manual(values = c("#0D0887FF", "#A92395FF", "#F89441FF", "#FDC328FF", "#FCFFA4FF")) +
  labs(title = "Box plot", 
       x = "Biomass",
       y = "")

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

#write_xlsx(summary_data, "output/normalized_biomass_number.xlsx")


# TO save summary
mean_data <- yesdata %>%
  group_by(food, series) %>%
  summarize(
    mean_biomass = mean(biomass, na.rm = TRUE),
    se_biomass = sd(biomass, na.rm = TRUE) / sqrt(n()),
    mean_number = mean(number, na.rm = TRUE),
    se_number = sd(number, na.rm = TRUE) / sqrt(n())
  )

#write_xlsx(mean_data, "output/average_biomass_number.xlsx")

# View the summary data
print(summary_data)


# Step 3: Plot normalized values with error bars for each series
biomass <- ggplot(summary_data, aes(x = mean_norm_biomass, y = reorder(food, mean_norm_biomass))) + 
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "black"
  ) +
  geom_crossbar(aes(xmin = mean_norm_biomass - se_norm_biomass, xmax = mean_norm_biomass + se_norm_biomass, fill = factor(series)), color = "black",
                fatten = 0.7, position = position_dodge(width = 0.6), width = 0.5) +
  #geom_point(size = 2) + 
  xlim(0,1.6) +
  theme_minimal() +
  #scale_color_manual(values = c("#0D0887FF", "#A92395FF", "#F89441FF", "#FDC328FF", "#FCFFA4FF")) +
  scale_fill_manual(values = c("#0D0887FF", "#A92395FF", "#F89441FF", "#FDC328FF", "#FCFFA4FF")) +
  labs(title = "Normalized Biomass by Treatment and Series",
       x = "Normalized Biomass (mean ± SE)",
       y = "",
       color = "Series") +
  theme(axis.text.y = element_text(size = 8),
        legend.position = "bottom")


#### No colouring according to series

# Option A

ggplot(summary_data, aes(x = mean_norm, y = reorder(food, mean_norm), group = series)) + 
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "black"
  ) +
  geom_crossbar(aes(xmin = mean_norm - se_norm, xmax = mean_norm + se_norm), color = "black", fill = "#F89441FF",
                fatten = 0.7, position = position_dodge(width = 0.6), width = 0.5) +
  #geom_point(size = 2) + 
  xlim(0,1.6) +
  theme_minimal() +
  #scale_color_manual(values = c("#0D0887FF", "#A92395FF", "#F89441FF", "#FDC328FF", "#FCFFA4FF")) +
  #scale_color_manual(values = "#F89441FF") +
  labs(title = "Normalized Biomass by Treatment and Series",
       x = "Normalized Biomass (mean ± SE)",
       y = "",
       color = "Series") +
  theme(axis.text.y = element_text(size = 8))


# Option B

ggplot() + 
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "black"
  ) +
  geom_errorbar(data = summary_data, aes(x = mean_norm, y = reorder(food, mean_norm), 
                                         xmin = mean_norm - se_norm, 
                                         xmax = mean_norm + se_norm, group = series), 
                color = "#F89441FF",
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_point(data = summary_data, aes(x = mean_norm, y = reorder(food, mean_norm), group = series), 
             shape = 21, fill = "#F89441FF", color = "black",
             position = position_dodge(width = 0.9)) + # Ensure same dodge position
  xlim(0, 1.6) +
  theme_minimal() +
  labs(title = "Normalized Biomass by Treatment and Series",
       x = "Normalized Biomass (mean ± SE)",
       y = "") +
  theme(axis.text.y = element_text(size = 8),
        plot.margin = margin(4, 2, 4, 1, "cm"))


# Option C

ggplot(summary_data, aes(y = reorder(food, mean_norm_biomass), x = mean_norm_biomass, fill = series)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), fill = "#F89441FF", color = "black") +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "black"
  ) +
  geom_errorbar(aes(xmin = mean_norm_biomass - se_norm_biomass, xmax = mean_norm_biomass + se_norm_biomass, group = series), 
                width = 0.2, position = position_dodge(width = 0.8), color = "black") +
  geom_point(aes(group = series), position = position_dodge(width = 0.8), 
             shape = 21, size = 2.5, fill = "white", color = "black") +
  labs(title = "Normalized Biomass by Treatment and Series",
       x = "Normalized Biomass (mean ± SE)",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.margin = margin(4, 2, 4, 1, "cm"),
        legend.position = "none")


# Step 3: Plot normalized values with error bars for each series
number <- ggplot(summary_data, aes(x = mean_norm, y = reorder(food, mean_norm))) + 
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "black"
  ) +
  geom_crossbar(aes(xmin = mean_norm - se_norm, xmax = mean_norm + se_norm, fill = factor(series)), color = "black",
                fatten = 0.7, position = position_dodge(width = 0.6), width = 0.5) +
  #geom_point(size = 2) + 
  theme_minimal() +
  #scale_color_manual(values = c("#0D0887FF", "#A92395FF", "#F89441FF", "#FDC328FF", "#FCFFA4FF")) +
  scale_fill_manual(values = c("#0D0887FF", "#A92395FF", "#F89441FF", "#FDC328FF", "#FCFFA4FF")) +
  labs(title = "Normalized Number of Juveniles by Treatment and Series",
       x = "Normalized Number (mean ± SE)",
       y = "",
       color = "Series") +
  theme(axis.text.y = element_text(size = 8),
        legend.position = "bottom")



ggplot(summary_data, aes(x = mean_norm, y = reorder(food, mean_norm), group = series)) + 
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "black"
  ) +
  geom_crossbar(aes(xmin = mean_norm - se_norm, xmax = mean_norm + se_norm), color = "black", fill = "#A92395FF",
                fatten = 0.7, position = position_dodge(width = 0.6), width = 0.5) +
  #geom_point(size = 2) + 
  #xlim(0,1.6) +
  theme_minimal() +
  #scale_color_manual(values = c("#0D0887FF", "#A92395FF", "#F89441FF", "#FDC328FF", "#FCFFA4FF")) +
  #scale_color_manual(values = "#F89441FF") +
  labs(title = "Normalized Number of Juveniles by Treatment and Series",
       x = "Normalized Number (mean ± SE)",
       y = "",
       color = "Series") +
  theme(axis.text.y = element_blank())


# Option B

ggplot() + 
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "black"
  ) +
  geom_errorbar(data = summary_data, aes(x = mean_norm, y = reorder(food, mean_norm), 
                                         xmin = mean_norm - se_norm, 
                                         xmax = mean_norm + se_norm, group = series), 
                color = "#A92395FF",
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_point(data = summary_data, aes(x = mean_norm, y = reorder(food, mean_norm), group = series), 
             shape = 21, fill = "#A92395FF", color = "black",
             position = position_dodge(width = 0.9)) + # Ensure same dodge position
  #xlim(0, 1.6) +
  theme_minimal() +
  labs(title = "Normalized Number of Juveniles by Treatment and Series",
       x = "Normalized Number (mean ± SE)",
       y = "") +
  theme(axis.text.y = element_text(size = 8),
        plot.margin = margin(4, 2, 4, 1, "cm"))


# Option C

ggplot(summary_data, aes(y = reorder(food, mean_norm_number), x = mean_norm_number, fill = series)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), fill = "#A92395FF", color = "black") +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "black"
  ) +
  geom_errorbar(aes(xmin = mean_norm_number - se_norm_number, xmax = mean_norm_number + se_norm_number, group = series), 
                width = 0.2, position = position_dodge(width = 0.8), color = "black") +
  geom_point(aes(group = series), position = position_dodge(width = 0.8), 
             shape = 21, size = 2.5, fill = "white", color = "black") +
  labs(title = "Normalized Number of Juveniles by Treatment and Series",
       x = "Normalized Number (mean ± SE)",
       y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        plot.margin = margin(4, 2, 4, 1, "cm"),
        legend.position = "none")



