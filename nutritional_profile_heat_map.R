# Code from Maja thesis on nutritional profile of Enchytraeus fed different treatments

# packages
pkgs <- c(
  "readxl",
  "dplyr",
  "fields",
  "plotrix",
  "tidyr",
  "ggplot2",
  "car",
  "tidyverse",
  "reshape2",
  "multcompView",
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


# Create summary data, remove NA observations
SumTab <- yesdata %>%
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
    pctLipid          = mean(pct_lipid),
    pctCarbohydrate   = mean(pct_carbohydrate)
  )


# For clarification: biomass (mg) and total Fa (mg/g dw)? To calculate total amount of FA
# we calculate biomass * mean 'Total FA'/1000 = total FA mg)

SumTab$amount_fa    <-  SumTab$mean_biomass * SumTab$mean_total_fa / 1000 # Generate Total FA amount form biomass


SumTab$biomass_per_worm <- SumTab$mean_biomass / SumTab$mean_number
SumTab$amount_protein <- (SumTab$mean_protein_percent_dw * SumTab$mean_biomass) /
  100 #Divided by 100 because protein is percent


##### Create dataset for heatmap#####
SumHeatmap <- SumTab


# Calculate normalized data for heatmaps (ie. divide by the mean of oatmeal within each series)
# All oatmeal should be normalized to be 1, but wine yest series A and wine yest series B might be a bit different,
# so all treatments are grouped by both series and food at first, and then summarized if the wine yest values are close enough
# Make a loop to calculate all normalized data
# (e.g. normalized sugar kelp biomass = mean sugar kelp biomass from series "i" / mean oatmeal biomass from that series "i")

SumHeatmap$norm_biomass = NA
SumHeatmap$norm_number = NA
SumHeatmap$norm_total_fa = NA
SumHeatmap$norm_biomass_per_worm = NA


# Calculate Normalized data
for (i in unique(SumHeatmap$series)) {
  SumHeatmap$norm_biomass[SumHeatmap$series == i]     <- SumHeatmap$mean_biomass[SumHeatmap$series ==
                                                                                   i] /
    SumHeatmap$mean_biomass[SumHeatmap$series == i &
                              SumHeatmap$food == 'Oatmeal']
  
  SumHeatmap$norm_number[SumHeatmap$series == i]      <- SumHeatmap$mean_number[SumHeatmap$series ==
                                                                                  i] /
    SumHeatmap$mean_number[SumHeatmap$series == i &
                             SumHeatmap$food == 'Oatmeal']
  
  SumHeatmap$norm_total_fa[SumHeatmap$series == i]    <- SumHeatmap$mean_total_fa[SumHeatmap$series ==
                                                                                    i] /
    SumHeatmap$mean_total_fa[SumHeatmap$series == i &
                               SumHeatmap$food == 'Oatmeal']
  
  SumHeatmap$norm_biomass_per_worm[SumHeatmap$series == i]    <- SumHeatmap$biomass_per_worm[SumHeatmap$series ==
                                                                                   i] /
    SumHeatmap$biomass_per_worm[SumHeatmap$series == i &
                               SumHeatmap$food == 'Oatmeal']
}

# Dataset with normalized data

df_norm <- SumHeatmap %>%
  select(food, starts_with("norm_"))


# Create summary table for food data, grouped by food
#Summary data table for the food protein, Total FA and Total O3 FA content
#FoodContentData <- read.xlsx("data/all_data.xlsx", sheetIndex = 4)
FoodContentData <- read_excel("data/all_data_maja.xlsx", sheet = "food_protein_fa")

FoodContentData %>%
  clean_names() -> FoodContentData

# For summary, remove NA observations
df_nutrientfood <- FoodContentData %>%
  group_by(food) %>%
  summarize(
    mean_pct_protein = mean(pct_protein_dw, na.rm = TRUE),
    sd_pct_protein = sd(pct_protein_dw, na.rm = TRUE),
    n_pct_protein = n_distinct(pct_protein_dw, na.rm = TRUE),
    mean_total_fa = mean(total_fa, na.rm = TRUE),
    sd_total_fa = sd(total_fa, na.rm = TRUE),
    n_total_fa = n_distinct(total_fa, na.rm = TRUE)
  )

# Write table out to Excel
#write.xlsx(SumTab4, file = paste(OutputPath, "Table_Summary_FoodContent.xlsx",sep=""),
#sheetName = "Summary", append = FALSE)

########################HEATMAP###############################################

# The fraction of nutrients (protein, carbohydrate, lipids) within each treatment,
# SO might as well work with mean values

SumHeatmap <- SumHeatmap %>%
  mutate(food = gsub("_", " ", food))

SumHeatmap$food[SumHeatmap$food == "Baking yeast"] <- "Bake Yeast"
SumHeatmap$food[SumHeatmap$food == "Oatmeal Wine Yeast Coffee Grind (33 33 33)"] <- "Oat-WY-CG 33:33:33"
SumHeatmap$food[SumHeatmap$food == "Oatmeal after 3 weeks"] <- "Oatmeal 3wk"
SumHeatmap$food[SumHeatmap$food == "Oatmeal Brewers grain (50 50)"] <- "Oat-BG 50:50"
SumHeatmap$food[SumHeatmap$food == "Oatmeal Coffee Grind (50 50)"] <- "Oat-CG 50:50"
SumHeatmap$food[SumHeatmap$food == "Wine yeast"] <- "Wine Yeast"
SumHeatmap$food[SumHeatmap$food == "Oatmeal Wine Yeast (50 50)"] <- "Oat-WY 50:50"
SumHeatmap$food[SumHeatmap$food == "Oatmeal Protein powder (66 33)"] <- "Oat-PP 66:33"
SumHeatmap$food[SumHeatmap$food == "Oatmeal Oil (80 20)"] <- "Oat-Oil 80:20"
SumHeatmap$food[SumHeatmap$food == "Oatmeal Protein powder (50 50)"] <- "Oat-PP 50:50"
SumHeatmap$food[SumHeatmap$food == "Fat reduced oatmeal"] <- "Low-fat Oat"
SumHeatmap$food[SumHeatmap$food == "Coffee Grind Protein powder (33 66)"] <- "CG-PP 33:66"
SumHeatmap$food[SumHeatmap$food == "Yeast extract"] <- "Yeast Extr."
SumHeatmap$food[SumHeatmap$food == "Coffee Grind Protein powder (66 33)"] <- "CG-PP 66:33"
SumHeatmap$food[SumHeatmap$food == "Brewers yeast"] <- "Brew Yeast"
SumHeatmap$food[SumHeatmap$food == "Brewers grain"] <- "Brew Grain"
SumHeatmap$food[SumHeatmap$food == "Protein powder"] <- "Prot. Powder"
SumHeatmap$food[SumHeatmap$food == "Sugar kelp"] <- "Sugar Kelp"
SumHeatmap$food[SumHeatmap$food == "Coffee Grind Oil (66 33)"] <- "CG-Oil 66:33"
SumHeatmap$food[SumHeatmap$food == "Potato starch"] <- "Pot. Starch"
SumHeatmap$food[SumHeatmap$food == "No feeding"] <- "No Feed"

PlotHeatmap <- SumHeatmap %>%
  dplyr::select(
    food,
    norm_biomass_per_worm,
    norm_biomass,
    norm_number,
    norm_total_fa,
    mean_biomass,
    mean_number,
    mean_total_fa,
    amount_protein,
    pctProtein,
    pctLipid,
    pctCarbohydrate
  )


#We're not really interested in biomass per worm

Data <- PlotHeatmap[!is.na(PlotHeatmap$pctProtein) &
                      !is.na(PlotHeatmap$pctCarbohydrate) &
                      !is.nan(PlotHeatmap$norm_biomass), ]


#We are most interested in heatmaps with the nutritional landscape as energy 
# contributions from macronutrients measured in each treatment 

Data$ProteinKcal      <- Data$pctProtein      * 4
Data$LipidKcal        <- Data$pctLipid        * 9
Data$CarbohydrateKcal <- Data$pctCarbohydrate * 4

# 100% energy
Data$TotalKcal <-  Data$ProteinKcal + Data$LipidKcal + Data$CarbohydrateKcal

# Calculating the percentage of energy contributed from each macronutrient
Data$PctKcalProtein <- Data$ProteinKcal / Data$TotalKcal *
  100
Data$PctKcalLipid <- Data$LipidKcal / Data$TotalKcal *
  100
Data$PctKcalCarbohydrate <- Data$CarbohydrateKcal / Data$TotalKcal *
  100

################################################################################ 

########################## Plot heatmap of biomass ############################

################################################################################

#X-variables: % Protein and % Carbohydrate are the predictors that will form the 2D surface of the thin-plate spline.
#Y-variable: norm_biomass is the dependent variable (or response) that we're trying to model based on the predictors.
#The Tps function in this case will fit a thin-plate spline to the relationship between % Protein and % Carbohydrate
#(the independent variables) and norm_biomass (the dependent variable), creating a smooth surface to describe
#how norm_biomass changes across the values of the two predictors.


par(mfrow = c(2, 2))

colors <- tim.colors(30, alpha = 1.0)  # Generate a color palette with 500 colors
color_mapping <- cut(Data$norm_biomass, breaks = 30, labels = FALSE)  # Map data to 50 colors

# Automatic lambda selection using GCV (Generalized cross-validation)
fit <- Tps(cbind(Data$PctKcalProtein, Data$PctKcalCarbohydrate),
           Data$norm_biomass,
           lambda = 0.02)
# Check the lambda chosen by GCV
print(fit$lambda)   # This is the optimal lambda value based on GCV


surface(
  fit,
  type = "I",
  lab = c(6, 6, 6),
  col = tim.colors(500),
  xlab = "% Protein",
  nx = 500,
  xlim = c(0, 100),
  # Type "I" is only color and not contour lines as in "C"
  ylab = "% Carbohydrate",
  ny = 500,
  ylim = c(0, 100),
  #main=paste(vNames[i]),
  font.lab = 1,
  font.axis = 1,
  labcex = 1.25,
  cex.axis = 1.25,
  cex.lab = 1.25,
  asp = 1
)              # Changing sizes
title(main = "Normalized Biomass of Worms \nlambda = 0.02")
lines(
  x = c(0, 100),
  y = c(100, 0),
  lty = 1,
  col = "gray50"
)                                # Making guide lines with text for lipid proportion
lines(
  x = c(0, 20),
  y = c(20, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 40),
  y = c(40, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 60),
  y = c(60, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 80),
  y = c(80, 0),
  lty = 2,
  col = "gray50"
)
text (
  x = 80,
  y = 5,
  labels = c("20"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 60,
  y = 5,
  labels = c("40"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 40,
  y = 5,
  labels = c("60"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 20,
  y = 5,
  labels = c("80"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 6,
  y = 6,
  labels = c("% Lipid "),
  srt = -45,
  cex = 1,
  col = "gray50"
)

points(
  Data$pctProtein,
  Data$pctCarbohydrate,
  pch = 21,
  bg = colors[color_mapping],
  col = "black",
  cex = 1.5
)  # pch sets the shape of the points

# Add treatment labels to the points (adjust position and size with `pos` and `cex`)
text(
  Data$pctProtein,
  Data$pctCarbohydrate,
  labels = Data$food,
  pos = 3,
  cex = 0.75,
  col = "black"
)

#Check model performance

summarize_tps <- function(fit) {
  list(
    coefficients = fit$d,
    lambda = fit$lambda,
    residual_summary = summary(fit$residuals),
    mse = mean(fit$residuals^2),
    r_squared = 1 - (sum(fit$residuals^2) / 
                       sum((fit$y - mean(fit$y))^2))
  )
}
tps_summary <- summarize_tps(fit)
print(tps_summary)


#Check which treatment is best
#Here, data points are tested, and not predictions/interpolations from the model
# Subset data for red zones
high_biomass_data <- Data[Data$norm_biomass >= 1, ]  # Define a threshold for "high biomass"
high_biomass_treatments <- unique(high_biomass_data$food)
print(high_biomass_treatments)


################################################################################ 

##################### Plot heatmap of number of juveniles ######################

################################################################################


colors <- tim.colors(30, alpha = 1.0)  # Generate a color palette with 500 colors
color_mapping <- cut(Data$norm_number, breaks = 30, labels = FALSE)  # Map data to 50 colors

# Automatic lambda selection using GCV (Generalized cross-validation)
fit <- Tps(cbind(Data$PctKcalProtein, Data$PctKcalCarbohydrate),
           Data$norm_number,
           lambda = 0.08)
# Check the lambda chosen by GCV
print(fit$lambda)   # This is the optimal lambda value based on GCV

surface(
  fit,
  type = "I",
  lab = c(6, 6, 6),
  col = tim.colors(500),
  xlab = "% Protein",
  nx = 500,
  xlim = c(0, 100),
  # Type "I" is only color and not contour lines as in "C"
  ylab = "% Carbohydrate",
  ny = 500,
  ylim = c(0, 100),
  #main=paste(vNames[i]),
  font.lab = 1,
  font.axis = 1,
  labcex = 1.25,
  cex.axis = 1.25,
  cex.lab = 1.25,
  asp = 1
)              # Changing sizes
title(main = "Normalized Number of juvenile Worms \nlambda = 0.08")
lines(
  x = c(0, 100),
  y = c(100, 0),
  lty = 1,
  col = "gray50"
)                                # Making guide lines with text for lipid proportion
lines(
  x = c(0, 20),
  y = c(20, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 40),
  y = c(40, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 60),
  y = c(60, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 80),
  y = c(80, 0),
  lty = 2,
  col = "gray50"
)
text (
  x = 80,
  y = 5,
  labels = c("20"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 60,
  y = 5,
  labels = c("40"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 40,
  y = 5,
  labels = c("60"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 20,
  y = 5,
  labels = c("80"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 6,
  y = 6,
  labels = c("% Lipid "),
  srt = -45,
  cex = 1,
  col = "gray50"
)

points(
  Data$pctProtein,
  Data$pctCarbohydrate,
  pch = 21,
  bg = colors[color_mapping],
  col = "black",
  cex = 1.5
)  # pch sets the shape of the points

# Add treatment labels to the points (adjust position and size with `pos` and `cex`)
text(
  Data$pctProtein,
  Data$pctCarbohydrate,
  labels = Data$food,
  pos = 3,
  cex = 0.75,
  col = "black"
)

#Check model performance

summarize_tps <- function(fit) {
  list(
    coefficients = fit$d,
    lambda = fit$lambda,
    residual_summary = summary(fit$residuals),
    mse = mean(fit$residuals^2),
    r_squared = 1 - (sum(fit$residuals^2) / 
                       sum((fit$y - mean(fit$y))^2))
  )
}
tps_summary <- summarize_tps(fit)
print(tps_summary)


#Check which treatment is best
#Here, data points are tested, and not predictions/interpolations from the model
# Subset data for red zones
high_number_data <- Data[Data$norm_number >= 1, ]  # Define a threshold for "high biomass"
high_number_treatments <- unique(high_number_data$food)
print(high_number_treatments)


################################################################################ 

######################### Plot heatmap of fatty acid ###########################

################################################################################


colors <- tim.colors(30, alpha = 1.0)  # Generate a color palette with 500 colors
color_mapping <- cut(Data$norm_total_fa, breaks = 30, labels = FALSE)  # Map data to 50 colors

# Automatic lambda selection using GCV (Generalized cross-validation)
fit <- Tps(cbind(Data$PctKcalProtein, Data$PctKcalCarbohydrate),
           Data$norm_total_fa,
           lambda = 0.004)
# Check the lambda chosen by GCV
print(fit$lambda)   # This is the optimal lambda value based on GCV

surface(
  fit,
  type = "I",
  lab = c(6, 6, 6),
  col = tim.colors(500),
  xlab = "% Protein",
  nx = 500,
  xlim = c(0, 100),
  # Type "I" is only color and not contour lines as in "C"
  ylab = "% Carbohydrate",
  ny = 500,
  ylim = c(0, 100),
  #main=paste(vNames[i]),
  font.lab = 1,
  font.axis = 1,
  labcex = 1.25,
  cex.axis = 1.25,
  cex.lab = 1.25,
  asp = 1
)              # Changing sizes
title(main = "Normalized fatty acid content in Worms \nlambda = 0.004")
lines(
  x = c(0, 100),
  y = c(100, 0),
  lty = 1,
  col = "gray50"
)                                # Making guide lines with text for lipid proportion
lines(
  x = c(0, 20),
  y = c(20, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 40),
  y = c(40, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 60),
  y = c(60, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 80),
  y = c(80, 0),
  lty = 2,
  col = "gray50"
)
text (
  x = 80,
  y = 5,
  labels = c("20"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 60,
  y = 5,
  labels = c("40"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 40,
  y = 5,
  labels = c("60"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 20,
  y = 5,
  labels = c("80"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 6,
  y = 6,
  labels = c("% Lipid "),
  srt = -45,
  cex = 1,
  col = "gray50"
)

points(
  Data$pctProtein,
  Data$pctCarbohydrate,
  pch = 21,
  bg = colors[color_mapping],
  col = "black",
  cex = 1.5
)  # pch sets the shape of the points

# Add treatment labels to the points (adjust position and size with `pos` and `cex`)
text(
  Data$pctProtein,
  Data$pctCarbohydrate,
  labels = Data$food,
  pos = 3,
  cex = 0.75,
  col = "black"
)

################################################################################
########################## Plot with contour lines #############################
################################################################################

################################################################################ 

########################## Plot heatmap of biomass ############################

################################################################################

#X-variables: % Protein and % Carbohydrate are the predictors that will form the 2D surface of the thin-plate spline.
#Y-variable: norm_biomass is the dependent variable (or response) that we're trying to model based on the predictors.
#The Tps function in this case will fit a thin-plate spline to the relationship between % Protein and % Carbohydrate
#(the independent variables) and norm_biomass (the dependent variable), creating a smooth surface to describe
#how norm_biomass changes across the values of the two predictors.


par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))


colors <- tim.colors(30, alpha = 1.0)  # Generate a color palette with 500 colors
color_mapping <- cut(Data$norm_biomass, breaks = 30, labels = FALSE)  # Map data to 50 colors

# Automatic lambda selection using GCV (Generalized cross-validation)
fit <- Tps(cbind(Data$PctKcalProtein, Data$PctKcalCarbohydrate),
           Data$norm_biomass,
           lambda = 0.02)
# Check the lambda chosen by GCV
print(fit$lambda)   # This is the optimal lambda value based on GCV


# Generate a prediction surface on a structured grid
grid_surface <- predictSurface(fit)

# First, plot the heatmap using 'surface()'
surface(
  fit,
  type = "I",  # Color heatmap without contour lines
  col = tim.colors(500),
  xlab = "% Protein",
  nx = 500,
  xlim = c(0, 100),
  ylab = "% Carbohydrate",
  ny = 500,
  ylim = c(0, 100),
  asp = 1
)

# Then, overlay contour lines using the structured grid from predictSurface()
contour(
  grid_surface$x, grid_surface$y, grid_surface$z,
  add = TRUE,   # Overlay on the existing heatmap
  col = "black",  # Contour line color
  lwd = 1,  # Line width
  lty = 2  # Dashed lines for contour
)
title(main = "Normalized Biomass of Worms \n λ = 0.02")
lines(
  x = c(0, 100),
  y = c(100, 0),
  lty = 1,
  col = "gray50"
)                                # Making guide lines with text for lipid proportion
lines(
  x = c(0, 20),
  y = c(20, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 40),
  y = c(40, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 60),
  y = c(60, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 80),
  y = c(80, 0),
  lty = 2,
  col = "gray50"
)
text (
  x = 80,
  y = 5,
  labels = c("20"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 60,
  y = 5,
  labels = c("40"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 40,
  y = 5,
  labels = c("60"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 20,
  y = 5,
  labels = c("80"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 6,
  y = 6,
  labels = c("% Lipid "),
  srt = -45,
  cex = 1,
  col = "gray50"
)

points(
  Data$pctProtein,
  Data$pctCarbohydrate,
  pch = 21,
  bg = colors[color_mapping],
  col = "black",
  cex = 1.5
)  # pch sets the shape of the points

# Add treatment labels to the points (adjust position and size with `pos` and `cex`)
text(
  Data$pctProtein,
  Data$pctCarbohydrate,
  labels = Data$food,
  pos = 3,
  cex = 0.75,
  col = "black"
)

#Check model performance

summarize_tps <- function(fit) {
  list(
    coefficients = fit$d,
    lambda = fit$lambda,
    residual_summary = summary(fit$residuals),
    mse = mean(fit$residuals^2),
    r_squared = 1 - (sum(fit$residuals^2) / 
                       sum((fit$y - mean(fit$y))^2))
  )
}
tps_summary <- summarize_tps(fit)
print(tps_summary)


#Check which treatment is best
#Here, data points are tested, and not predictions/interpolations from the model
# Subset data for red zones
high_biomass_data <- Data[Data$norm_biomass >= 1, ]  # Define a threshold for "high biomass"
high_biomass_treatments <- unique(high_biomass_data$food)
print(high_biomass_treatments)


################################################################################ 

##################### Plot heatmap of number of juveniles ######################

################################################################################


colors <- tim.colors(30, alpha = 1.0)  # Generate a color palette with 500 colors
color_mapping <- cut(Data$norm_number, breaks = 30, labels = FALSE)  # Map data to 50 colors

# Automatic lambda selection using GCV (Generalized cross-validation)
fit <- Tps(cbind(Data$PctKcalProtein, Data$PctKcalCarbohydrate),
           Data$norm_number,
           lambda = 0.08)
# Check the lambda chosen by GCV
print(fit$lambda)   # This is the optimal lambda value based on GCV

# Generate a prediction surface on a structured grid
grid_surface <- predictSurface(fit)

# First, plot the heatmap using 'surface()'
surface(
  fit,
  type = "I",  # Color heatmap without contour lines
  col = tim.colors(500),
  xlab = "% Protein",
  nx = 500,
  xlim = c(0, 100),
  ylab = "% Carbohydrate",
  ny = 500,
  ylim = c(0, 100),
  asp = 1
)

# Then, overlay contour lines using the structured grid from predictSurface()
contour(
  grid_surface$x, grid_surface$y, grid_surface$z,
  add = TRUE,   # Overlay on the existing heatmap
  col = "black",  # Contour line color
  lwd = 1,  # Line width
  lty = 2  # Dashed lines for contour
)
title(main = "Normalized Number of juvenile Worms \n λ = 0.08")
lines(
  x = c(0, 100),
  y = c(100, 0),
  lty = 1,
  col = "gray50"
)                                # Making guide lines with text for lipid proportion
lines(
  x = c(0, 20),
  y = c(20, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 40),
  y = c(40, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 60),
  y = c(60, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 80),
  y = c(80, 0),
  lty = 2,
  col = "gray50"
)
text (
  x = 80,
  y = 5,
  labels = c("20"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 60,
  y = 5,
  labels = c("40"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 40,
  y = 5,
  labels = c("60"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 20,
  y = 5,
  labels = c("80"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 6,
  y = 6,
  labels = c("% Lipid "),
  srt = -45,
  cex = 1,
  col = "gray50"
)

points(
  Data$pctProtein,
  Data$pctCarbohydrate,
  pch = 21,
  bg = colors[color_mapping],
  col = "black",
  cex = 1.5
)  # pch sets the shape of the points

# Add treatment labels to the points (adjust position and size with `pos` and `cex`)
text(
  Data$pctProtein,
  Data$pctCarbohydrate,
  labels = Data$food,
  pos = 3,
  cex = 0.75,
  col = "black"
)

#Check model performance

summarize_tps <- function(fit) {
  list(
    coefficients = fit$d,
    lambda = fit$lambda,
    residual_summary = summary(fit$residuals),
    mse = mean(fit$residuals^2),
    r_squared = 1 - (sum(fit$residuals^2) / 
                       sum((fit$y - mean(fit$y))^2))
  )
}
tps_summary <- summarize_tps(fit)
print(tps_summary)


#Check which treatment is best
#Here, data points are tested, and not predictions/interpolations from the model
# Subset data for red zones
high_number_data <- Data[Data$norm_number >= 1, ]  # Define a threshold for "high biomass"
high_number_treatments <- unique(high_number_data$food)
print(high_number_treatments)


################################################################################ 

######################### Plot heatmap of fatty acid ###########################

################################################################################


colors <- tim.colors(30, alpha = 1.0)  # Generate a color palette with 500 colors
color_mapping <- cut(Data$norm_total_fa, breaks = 30, labels = FALSE)  # Map data to 50 colors

# Automatic lambda selection using GCV (Generalized cross-validation)
fit <- Tps(cbind(Data$PctKcalProtein, Data$PctKcalCarbohydrate),
           Data$norm_total_fa,
           lambda = 0.004)
# Check the lambda chosen by GCV
print(fit$lambda)   # This is the optimal lambda value based on GCV

# Generate a prediction surface on a structured grid
grid_surface <- predictSurface(fit)

# First, plot the heatmap using 'surface()'
surface(
  fit,
  type = "I",  # Color heatmap without contour lines
  col = tim.colors(500),
  xlab = "% Protein",
  nx = 500,
  xlim = c(0, 100),
  ylab = "% Carbohydrate",
  ny = 500,
  ylim = c(0, 100),
  asp = 1
)

# Then, overlay contour lines using the structured grid from predictSurface()
contour(
  grid_surface$x, grid_surface$y, grid_surface$z,
  add = TRUE,   # Overlay on the existing heatmap
  col = "black",  # Contour line color
  lwd = 1,  # Line width
  lty = 2  # Dashed lines for contour
)
title(main = "Normalized fatty acid content in Worms \n λ = 0.004")
lines(
  x = c(0, 100),
  y = c(100, 0),
  lty = 1,
  col = "gray50"
)                                # Making guide lines with text for lipid proportion
lines(
  x = c(0, 20),
  y = c(20, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 40),
  y = c(40, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 60),
  y = c(60, 0),
  lty = 2,
  col = "gray50"
)
lines(
  x = c(0, 80),
  y = c(80, 0),
  lty = 2,
  col = "gray50"
)
text (
  x = 80,
  y = 5,
  labels = c("20"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 60,
  y = 5,
  labels = c("40"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 40,
  y = 5,
  labels = c("60"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 20,
  y = 5,
  labels = c("80"),
  srt = -45,
  cex = 1,
  col = "gray50"
)
text (
  x = 6,
  y = 6,
  labels = c("% Lipid "),
  srt = -45,
  cex = 1,
  col = "gray50"
)

points(
  Data$pctProtein,
  Data$pctCarbohydrate,
  pch = 21,
  bg = colors[color_mapping],
  col = "black",
  cex = 1.5
)  # pch sets the shape of the points

# Add treatment labels to the points (adjust position and size with `pos` and `cex`)
text(
  Data$pctProtein,
  Data$pctCarbohydrate,
  labels = Data$food,
  pos = 3,
  cex = 0.75,
  col = "black"
)

################################################################################



#Check model performance

summarize_tps <- function(fit) {
  list(
    coefficients = fit$d,
    lambda = fit$lambda,
    residual_summary = summary(fit$residuals),
    mse = mean(fit$residuals^2),
    r_squared = 1 - (sum(fit$residuals^2) / 
                       sum((fit$y - mean(fit$y))^2))
  )
}
tps_summary <- summarize_tps(fit)
print(tps_summary)


#Check which treatment is best
#Here, data points are tested, and not predictions/interpolations from the model
# Subset data for red zones
high_number_data <- Data[Data$norm_number >= 1, ]  # Define a threshold for "high biomass"
high_number_treatments <- unique(high_number_data$food)
print(high_number_treatments)

dev.off()

# Check lambda for all response variables

Data <- PlotHeatmap[!is.na(PlotHeatmap$pctProtein) &
                      !is.na(PlotHeatmap$pctCarbohydrate) &
                      !is.nan(PlotHeatmap$norm_number), ]
fit <- Tps(cbind(Data$pctProtein, Data$pctCarbohydrate),
           Data$norm_number)
print(fit$lambda)

fit <- Tps(cbind(Data$pctProtein, Data$pctCarbohydrate),
           Data$norm_total_fa)
print(fit$lambda)

#Data <- PlotHeatmap[!is.nan(PlotHeatmap$norm_biomass), ]
fit <- Tps(cbind(Data$pctProtein, Data$pctCarbohydrate),
           Data$norm_biomass)
print(fit$lambda)


##########################Model validation and statistics#######################

# Check the lambda chosen by GCV
print(fit$lambda)   # This is the optimal lambda value based on GCV

lambda_values <- c(0.001, 0.01, 0.1, 0.5, 1, 10)
for (lambda in lambda_values) {
  fit <- Tps(cbind(Data$pctProtein, Data$pctCarbohydrate),
             Data$norm_number,
             lambda = lambda)
  plot(fit)  # Visualize the result to inspect the fit
  title(main = paste("Lambda =", lambda))
}

#### Residual distribution #####

#### BIOMASS ####

# Sample residuals based on the summary provided
residuals <- c(-0.332199, -0.128529, 0.006138, 0.163173, 0.531582)
residuals2 <- c(-0.46825, -0.05288, 0.04822, 0.08638, 0.36857)
residuals3 <- c(-0.394311, -0.094650, 0.002904, 0.102203, 0.460507)

# Create a histogram of residuals
hist(residuals, 
     breaks = 10, 
     col = "skyblue", 
     border = "black", 
     probability = TRUE, 
     main = "Residual Distribution", 
     xlab = "Residuals", 
     ylab = "Density")

# Add a density curve for the residuals
lines(density(residuals), col = "blue", lwd = 2)

# Overlay a normal distribution for comparison
x_vals <- seq(min(residuals) - 0.1, max(residuals) + 0.1, length = 100)
normal_curve <- dnorm(x_vals, mean = mean(residuals), sd = sd(residuals))
lines(x_vals, normal_curve, col = "red", lwd = 2)

# Add legend
legend("topright", legend = c("Density Curve", "Normal Distribution Fit"), 
       col = c("blue", "red"), lwd = 2)



hist(residuals2, 
     breaks = 10, 
     col = "skyblue", 
     border = "black", 
     probability = TRUE, 
     main = "Residual Distribution", 
     xlab = "Residuals", 
     ylab = "Density")

# Add a density curve for the residuals
lines(density(residuals2), col = "blue", lwd = 2)

# Overlay a normal distribution for comparison
x_vals <- seq(min(residuals2) - 0.1, max(residuals2) + 0.1, length = 100)
normal_curve <- dnorm(x_vals, mean = mean(residuals2), sd = sd(residuals2))
lines(x_vals, normal_curve, col = "red", lwd = 2)

# Add legend
legend("topright", legend = c("Density Curve", "Normal Distribution Fit"), 
       col = c("blue", "red"), lwd = 2)


hist(residuals3, 
     breaks = 10, 
     col = "skyblue", 
     border = "black", 
     probability = TRUE, 
     main = "Residual Distribution", 
     xlab = "Residuals", 
     ylab = "Density")

# Add a density curve for the residuals
lines(density(residuals3), col = "blue", lwd = 2)

# Overlay a normal distribution for comparison
x_vals <- seq(min(residuals3) - 0.1, max(residuals3) + 0.1, length = 100)
normal_curve <- dnorm(x_vals, mean = mean(residuals3), sd = sd(residuals3))
lines(x_vals, normal_curve, col = "red", lwd = 2)

# Add legend
legend("topright", legend = c("Density Curve", "Normal Distribution Fit"), 
       col = c("blue", "red"), lwd = 2)


############################ STATISTICS ########################################
# Load the mgcv package for GAM
library(mgcv)
library(statmod)

# Fit a GAM for normalized biomass
# Tweedie model
gam_tweedie <- gam(norm_biomass ~ s(PctKcalProtein) + s(PctKcalCarbohydrate) + s(PctKcalLipid), 
                   family = tw(link = "log"), 
                   data = Data)

# Gamma model
gam_gamma <- gam(norm_biomass ~ s(PctKcalProtein) + s(PctKcalCarbohydrate) + s(PctKcalLipid), 
                 family = Gamma(link = "log"), 
                 data = Data)

gam_gamma.1 <- gam(norm_biomass ~ s(PctKcalProtein) + s(PctKcalCarbohydrate), 
                   family = Gamma(link = "log"), 
                   data = Data)

gam.1 <- gam(norm_biomass ~ s(PctKcalProtein) + s(PctKcalCarbohydrate) + s(PctKcalLipid), 
                   data = Data)

AIC(gam_tweedie, gam_gamma, gam_gamma.1, gam.1)

summary(gam_gamma)
# Plot the smooth terms from the GAM model
plot(gam_gamma, pages = 1, rug = TRUE)

hist(residuals(gam_gamma), breaks = 20, main = "Residuals Histogram", xlab = "Residuals")

plot(fitted(gam_gamma), residuals(gam_gamma),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lwd = 2)

gam.check(gam_gamma)

acf(residuals(gam_gamma))


# Fit a GAM for normalized juvenile production
#gam_juveniles <- gam(norm_number ~ s(PctKcalCarbohydrate), data = Data)

gam_tweedie <- gam(norm_number ~ s(PctKcalProtein) + s(PctKcalCarbohydrate) + s(PctKcalLipid), 
                   family = tw(link = "log"), 
                   data = Data)

# Gamma model
gam_gamma <- gam(norm_number ~ s(PctKcalProtein) + s(PctKcalCarbohydrate) + s(PctKcalLipid), 
                 family = Gamma(link = "log"), 
                 data = Data)

gam_gamma.1 <- gam(norm_number ~ s(PctKcalProtein) + s(PctKcalCarbohydrate), 
                   family = Gamma(link = "log"), 
                   data = Data)

gam.1 <- gam(norm_number ~ s(PctKcalProtein) + s(PctKcalCarbohydrate) + s(PctKcalLipid), 
             data = Data)

gam.2 <- gam(norm_number ~ s(PctKcalCarbohydrate) + s(PctKcalProtein), 
             data = Data)

gam.3 <- gam(norm_number ~ s(PctKcalProtein), 
             data = Data)

AIC(gam_tweedie, gam_gamma, gam_gamma.1, gam.1, gam.2)

AIC(gam.1, gam.2, gam.3)

summary(gam.2)

# Plot the smooth terms from the GAM model
plot(gam.2, pages = 1, rug = TRUE)

hist(residuals(gam.2), breaks = 20, main = "Residuals Histogram", xlab = "Residuals")

plot(fitted(gam.2), residuals(gam.2),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lwd = 2)

gam.check(gam.2)

acf(residuals(gam.2))

# Fit a GAM for fatty acid content

gam.1 <- gam(norm_total_fa ~ s(PctKcalProtein) + s(PctKcalCarbohydrate) + s(PctKcalLipid), 
             data = Data)

gam.2 <- gam(norm_total_fa ~ s(PctKcalLipid), data = Data)

gam.3 <- gam(norm_total_fa ~ s(PctKcalCarbohydrate), data = Data)

AIC(gam.1, gam.2)

# Summary of each model to check statistical significance
summary(gam.2)
# Plot the smooth terms from the GAM model
plot(gam.2, pages = 1, rug = TRUE)

hist(residuals(gam.2), breaks = 20, main = "Residuals Histogram", xlab = "Residuals")

plot(fitted(gam.2), residuals(gam.2),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lwd = 2)

gam.check(gam.2)

acf(residuals(gam.2))

###############################################################################
#####         Correlation between normalized biomass per worm           #######
#####                       and amount of worms                         #######
###############################################################################


library(dplyr)

df_nutrientfood$food[df_nutrientfood$food == "Baking yeast"] <- "Bake Yeast"
df_nutrientfood$food[df_nutrientfood$food == "Brewer's grain"] <- "Brew Grain"
df_nutrientfood$food[df_nutrientfood$food == "Brewer's yeast"] <- "Brew Yeast"
df_nutrientfood$food[df_nutrientfood$food == "Fat-reduced oatmeal"] <- "Low-fat Oat"
df_nutrientfood$food[df_nutrientfood$food == "Potato starch"] <- "Pot. Starch"
df_nutrientfood$food[df_nutrientfood$food == "Protein powder"] <- "Prot. Powder"
df_nutrientfood$food[df_nutrientfood$food == "Sugar kelp"] <- "Sugar Kelp"
df_nutrientfood$food[df_nutrientfood$food == "Wine yeast"] <- "Wine Yeast"
df_nutrientfood$food[df_nutrientfood$food == "Yeast extract"] <- "Yeast Extr."


# Assuming 'feed_treatment' is the common column in both dataframes
Data <- Data %>%
  left_join(df_nutrientfood %>% dplyr::select(food, mean_pct_protein),
            by = "food")




# Scatter plot with a fitted line
biomass_per_worm <- ggplot(Data, aes(x = norm_number, y = norm_biomass_per_worm)) +
  geom_point(color = "blue",
              size = 3,
              alpha = 0.6) +
  ylim(0,2) +
  #scale_color_gradient(low = "blue", high = "red") +  # Adjust colors as needed
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(x = "Number of Worms", y = "Biomass per Worm") +
  theme_bw() +
  theme(legend.position = "none")

# Scatter plot with a fitted line
biomass_all <- ggplot(Data, aes(x = norm_number, y = norm_biomass)) +
  geom_point(color = "blue",
            size = 3,
            alpha = 0.6) +
  ylim(0,2) +
  #scale_color_gradient(low = "blue", high = "red") +  # Adjust colors as needed
  geom_smooth(method = "lm", color = "black", se = FALSE, linetype = "dashed") +
  labs(x = "Number of Worms", y = "Biomass of Worms") +
  theme_bw() +
  theme(legend.position = "none")

biomass_protein <- ggplot(Data, aes(x = pctProtein, y = norm_biomass_per_worm, color = mean_pct_protein)) +
  geom_point(size = 3, alpha = 0.6) +
  ylim(0, 2) +
  scale_color_gradient(low = "blue", high = "red", na.value = "grey") +  # NA values in grey
  geom_smooth(data = Data, aes(x = mean_pct_protein, y = norm_biomass_per_worm), 
              method = "lm", color = "black", se = FALSE) +  # Regression line uses mean_pct_protein.y
  labs(x = "% Protein in Feed", y = "Biomass per Worm", color = "Protein Content (%)") +
  theme_bw() 




require(ggpubr)

ggarrange(biomass_per_worm, biomass_all, biomass_protein, labels = c("A", "B", "C"), vjust = -1, hjust = -1.5) +
  theme(plot.margin = margin(5.5, 1, 5.5, 1, "cm"))

# Same, but different approach

# Calculate correlation
cor_value <- cor(Data$norm_biomass_per_worm, Data$norm_number, use = "complete.obs")

# Plot with correlation annotation
ggplot(Data, aes(x = norm_number, y = norm_biomass_per_worm)) +
  geom_point(color = "blue",
             size = 3,
             alpha = 0.6) +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Correlation between Biomass per Worm and Amount of Worms", x = "Amount of Worms", y = "Biomass of Worms") +
  annotate(
    "text",
    x = 1,
    y = 1.8,
    label = paste("Correlation:", round(cor_value, 2)),
    size = 5,
    color = "black"
  ) +
  theme_minimal()

# Pearson correlation test
cor_test_result <- cor.test(Data$norm_biomass_per_worm, Data$norm_number, method = "pearson")

# Print the results
print(cor_test_result)

cor_test_result <- cor.test(Data$norm_biomass, Data$norm_number, method = "pearson")

print(cor_test_result)

cor_test_result <- cor.test(Data$pctProtein, Data$norm_biomass_per_worm, method = "pearson")

print(cor_test_result)

lm <- lm(norm_biomass_per_worm ~ pctProtein, data = Data)
summary(lm)

lm <- lm(norm_biomass ~ norm_number, data = Data)
summary(lm)

lm <- lm(norm_biomass_per_worm ~ norm_number, data = Data)
summary(lm)

lm <- lm(norm_biomass_per_worm ~ mean_pct_protein.y, data = Data)
summary(lm)





