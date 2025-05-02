# Load necessary libraries
#install.packages("fields")

# Load necessary library
library(fields)

# Set a seed for reproducibility
set.seed(123)

# Example data with carbohydrate, protein, and biomass production
data <- data.frame(
  carbohydrate = runif(30, min = 10, max = 60),  # Carbohydrate %
  protein = runif(30, min = 5, max = 40),        # Protein %
  biomass = runif(30, min = 0.5, max = 2.0)      # Biomass
)

# Fit a Thin Plate Spline model
tps_model <- Tps(data[, c("carbohydrate", "protein")], data$biomass)

# Create a grid for carbohydrate and protein for visualization
grid_x <- seq(min(data$carbohydrate), max(data$carbohydrate), length.out = 100)
grid_y <- seq(min(data$protein), max(data$protein), length.out = 100)
grid <- expand.grid(carbohydrate = grid_x, protein = grid_y)

# Predict biomass over the grid using the Tps model
biomass_pred <- predict(tps_model, newdata = grid)

# Reshape predictions to a matrix for surface plotting
z_matrix <- matrix(biomass_pred, nrow = length(grid_y), ncol = length(grid_x))  # Ensure correct dimensions

# Check dimensions to avoid errors
print(dim(z_matrix))   # Should be 100 x 100
print(length(grid_x))  # Should be 100
print(length(grid_y))  # Should be 100

# Plot the surface using persp for better control
persp(grid_x, grid_y, z_matrix,
      xlab = "Carbohydrate (%)", 
      ylab = "Protein (%)", 
      zlab = "Biomass", 
      main = "Surface Plot of Biomass Production",
      theta = 30,           # Angle of rotation for viewing
      phi = 30,             # Angle of elevation for viewing
      col = "lightblue",    # Surface color
      ltheta = 120,         # Light direction
      shade = 0.5,          # Shading
      expand = 0.5)         # Expansion factor

image.plot(grid_x, grid_y, z_matrix,
           xlab = "Carbohydrate (%)", 
           ylab = "Protein (%)", 
           main = "Heatmap of Biomass Production",
           col = terrain.colors(100))
surface()
