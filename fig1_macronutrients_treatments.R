
# Figure 1 - macronutrients in food treatments

library(readxl)
library(janitor)
library(tidyverse)

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

# Create summary data, remove NA observations
SumTab1 <- yesdata %>%
  group_by(food, series) %>%
  summarize(
    mean_biomass = mean(biomass, na.rm = TRUE),
    sd_biomass = sd(biomass, na.rm = TRUE),
    n_biomass = n_distinct(biomass, na.rm = TRUE),
    # Remove missing values
    mean_number = mean(number, na.rm = TRUE),
    sd_number = sd(number, na.rm = TRUE),
    n_number = n_distinct(number, na.rm = TRUE),
    mean_protein_percent_dw = mean(protein_percent_dw, na.rm = TRUE),
    sd_protein_percent_dw = sd(protein_percent_dw, na.rm = TRUE),
    n_protein_percent_dw = n_distinct(protein_percent_dw, na.rm = TRUE),
    mean_ash_percent_dw = mean(ash_percent_dw, na.rm = TRUE),
    sd_ash_percent_dw = sd(ash_percent_dw, na.rm = TRUE),
    n_ash_percent_dw = n_distinct(ash_percent_dw, na.rm = TRUE),
    mean_total_fa = mean(total_fa, na.rm = TRUE),
    sd_total_fa = sd(total_fa, na.rm = TRUE),
    n_total_fa= n_distinct(total_fa, na.rm = TRUE),
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
    pctLipid          = mean(pct_lipid),
    pctCarbohydrate   = mean(pct_carbohydrate)
  )

TreatmentOrder <- factor(c("Oatmeal", "Oatmeal_Oil_(80_20)", "Oatmeal_Protein_powder_(66_33)",
                           "Oatmeal_Protein_powder_(50_50)", "Coffee_Grind_Oil_(66_33)", 
                           "Coffee_Grind_Protein_powder_(66_33)", "Coffee_Grind_Protein_powder_(33_66)",
                           "Potato_starch", "Sugar_kelp", "Wine_yeast", "Yeast_extract", 
                           "Coffee_Grind", "Protein_powder", "Brewers_grain", "Brewers_yeast", 
                           "Baking_yeast", "Fat_reduced_oatmeal", "No_feeding", 
                           "Oatmeal_after_3_weeks", "Oatmeal_Brewers_grain_(50_50)", 
                           "Oatmeal_Wine_Yeast_(50_50)", "Oatmeal_Coffee_Grind_(50_50)", 
                           "Oatmeal_Wine_Yeast_Coffee_Grind_(33_33_33)"))

#TreatmentOrder <- factor(c("Oatmeal", "Potato_starch", "Sugar_kelp", "Wine_yeast", "Yeast_extract", 
#                          "Coffee_Grind", "Protein_powder", "Brewers_grain", "Brewers_yeast", 
#                          "Baking_yeast", "Fat_reduced_oatmeal"))
#

# Creating a data frame, so the desired treatment order can be applied
globalOrder         <- data.frame(food = TreatmentOrder) # Create new data frame
globalOrder$listNum <- 1:length(TreatmentOrder)          # Make a list of numbers fitting the order


PlotData1 <- SumTab1 %>%
  dplyr::select(food, pctProtein, pctCarbohydrate, pctLipid)

PlotData1$ProteinKcal      <- PlotData1$pctProtein      * 4
PlotData1$LipidKcal        <- PlotData1$pctLipid        * 9
PlotData1$CarbohydrateKcal <- PlotData1$pctCarbohydrate * 4

# 100% energy
PlotData1$TotalKcal <-  PlotData1$ProteinKcal + PlotData1$LipidKcal + PlotData1$CarbohydrateKcal

# Calculating the percentage of energy contributed from each macronutrient
PlotData1$PctKcalProtein <- PlotData1$ProteinKcal / PlotData1$TotalKcal *
  100
PlotData1$PctKcalLipid <- PlotData1$LipidKcal / PlotData1$TotalKcal *
  100
PlotData1$PctKcalCarbohydrate <- PlotData1$CarbohydrateKcal / PlotData1$TotalKcal *
  100




PlotData1 <- PlotData1[rowSums(is.na(PlotData1))==0,] 
PlotData2 <- merge(PlotData1,globalOrder,id=food)     # Merging data with the ordered data
PlotData2 <- PlotData2[order(PlotData2$listNum),]     # Order the data as the listNum variable defined before

# Define unique colors and shapes for each treatment
palette <- c("orange", "orange", "orange", "firebrick2", "firebrick2", "firebrick2", "firebrick2", "brown",
             "brown","brown","brown","brown","brown","brown","brown","brown",
             "hotpink", "hotpink","hotpink","hotpink","hotpink")

palette <- viridis(21)

#palette <- c("orange", "firebrick2", "firebrick2", "brown",
#           "brown","brown","brown","brown","brown","brown",
#           "hotpink")

#pchtype <- c(15, 16, 17, 18, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 24, 25, 22, 21)
pchtype <- c(22, 22, 22, 25, 25, 25, 25, 24, 24, 24, 24, 24, 24, 24, 24, 24, 23, 23, 23, 23, 23)

#pchtype <- c(15, 16, 16, 17, 17, 17, 17, 17, 17, 17, 18)


# Create the ggplot
require(stringr)  # To use str_replace_all if needed

ggplot(PlotData2, aes(x = PctKcalProtein, y = PctKcalCarbohydrate, fill = food, shape = food)) +
  
  # Add points with custom size
  geom_point(size = 3) +
  
  # Customize color and shapes based on palette and pchtype, and replace underscores with spaces in labels
  scale_fill_manual(values = palette, labels = function(x) str_replace_all(x, "_", " ")) +
  scale_shape_manual(values = pchtype, labels = function(x) str_replace_all(x, "_", " ")) +
  
  # Add the guide lines
  geom_abline(slope = -1, intercept = 100, linetype = "solid", color = "gray50") +
  geom_abline(slope = -1, intercept = 20, linetype = "dashed", color = "gray50") +
  geom_abline(slope = -1, intercept = 40, linetype = "dashed", color = "gray50") +
  geom_abline(slope = -1, intercept = 60, linetype = "dashed", color = "gray50") +
  geom_abline(slope = -1, intercept = 80, linetype = "dashed", color = "gray50") +
  
  # Customize axis labels and title
  labs(title = "% Macronutrient energy contribution in treatments",
       x = "% Protein",
       y = "% Carbohydrate",
       color = "Treatment type",
       shape = "Treatment type") +
  xlim(0, 100) +
  ylim(0, 100) +
  
  # Add lipid percentage labels (rotated text)
  annotate("text", x = 80, y = 5, label = "20", angle = -45, size = 3, color = "gray50") +
  annotate("text", x = 60, y = 5, label = "40", angle = -45, size = 3, color = "gray50") +
  annotate("text", x = 40, y = 5, label = "60", angle = -45, size = 3, color = "gray50") +
  annotate("text", x = 20, y = 5, label = "80", angle = -45, size = 3, color = "gray50") +
  annotate("text", x = 6, y = 6, label = "% Lipid", angle = -45, size = 3, color = "gray50") +
  
  # Customize the legend as a single column on the right side
  guides(color = guide_legend(ncol = 3),
         shape = guide_legend(ncol = 3)) +
  
  # Customize theme for the plot
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.key.height = unit(0.1, "cm"),  # Adjust vertical spacing
        plot.margin = margin(5,3,5,3, "cm"))

############################Legend simplified##################################

# Rename Coffee_Grind_Protein_powder_(33_66) to Protein_powder_Coffee_Grind_(66_33)
PlotData2$food[PlotData2$food == "Protein_powder"] <- "Prot_pow"
PlotData2$food[PlotData2$food == "Coffee_Grind_Protein_powder_(33_66)"] <- "Prot_pow_Cof_Gri_(66_33)"


# Simplify treatment categories (grouping into broader categories)
PlotData2$food_group <- ifelse(grepl("Oatmeal", PlotData2$food), "Oatmeal-Based",
                               ifelse(grepl("Prot_pow|Prot_pow_Cof_Gri_(66_33)", PlotData2$food), "Protein-Based",
                                      ifelse(grepl("Coffee_Grind|Coffee_Grind_Oil_(66_33)|Coffee_Grind_Protein_powder_(66_33)", PlotData2$food), "Coffee-Based",
                                             ifelse(grepl("Yeast|Baking_yeast|Brewers_yeast|Wine_yeast", PlotData2$food), "Yeast-Based",
                                                    "Other"))))

library(viridis)

# Re-define palette and shape based on simplified categories
# Define viridis-based color palette
palette_simplified <- c("Oatmeal-Based" = viridis(4)[1],
                        "Coffee-Based"  = viridis(4)[2],
                        "Yeast-Based"   = viridis(4)[3],
                        "Protein-Based" = viridis(4)[4],
                        "Other"         = magma(5)[3])

# Updated point shapes
pchtype_simplified <- c("Oatmeal-Based" = 22,
                        "Coffee-Based"  = 25,
                        "Yeast-Based"   = 24,
                        "Protein-Based" = 23,
                        "Other"         = 21)


# Updated plot with simplified legend
# Plot with fill instead of color
ggplot(PlotData2, aes(x = PctKcalProtein, y = PctKcalCarbohydrate, fill = food_group, shape = food_group)) +
  
  geom_abline(slope = -1, intercept = 100, linetype = "solid", color = "gray50") +
  geom_abline(slope = -1, intercept = 20, linetype = "dashed", color = "gray50") +
  geom_abline(slope = -1, intercept = 40, linetype = "dashed", color = "gray50") +
  geom_abline(slope = -1, intercept = 60, linetype = "dashed", color = "gray50") +
  geom_abline(slope = -1, intercept = 80, linetype = "dashed", color = "gray50") +
  
  labs(title = "% Macronutrient energy contribution in treatments",
       x = "% Protein",
       y = "% Carbohydrate",
       fill = "Treatment Group",
       shape = "Treatment Group") +
  xlim(0, 100) +
  ylim(0, 100) +
  
  annotate("text", x = 80, y = 5, label = "20", angle = -45, size = 3, color = "gray50") +
  annotate("text", x = 60, y = 5, label = "40", angle = -45, size = 3, color = "gray50") +
  annotate("text", x = 40, y = 5, label = "60", angle = -45, size = 3, color = "gray50") +
  annotate("text", x = 20, y = 5, label = "80", angle = -45, size = 3, color = "gray50") +
  annotate("text", x = 5, y = 5, label = "% Lipid", angle = -45, size = 3, color = "gray50") +
  
  geom_point(size = 3, color = "black") +  # add black border to filled shapes
  
  scale_fill_manual(values = palette_simplified) +
  scale_shape_manual(values = pchtype_simplified) +
  
  
  guides(fill = guide_legend(title.position = "top"),
         shape = guide_legend(title.position = "top")) +
  
  coord_fixed() +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.margin = margin(2, 2, 2, 2, "cm"))



#Save output in excel
require(writexl)
#write_xlsx(PlotData2, "output/energy_contribution_macronutrients.xlsx")





