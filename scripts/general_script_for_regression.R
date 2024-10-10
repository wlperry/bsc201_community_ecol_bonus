# this file is set up to be able to run a regression by entering the data in lines 19 and 20

# Load libraries
library(readxl)
library(janitor)
library(skimr)
library(tidyverse)

# read in the file ------
# this is the same for all in bsc201
data.df <- read_excel("data/BSC201_ComEco_Bonus_Data.xlsx", sheet ="current_data") |> 
  clean_names()

# # # # # # ENTER BELOW # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# enter x and y variables ------
# below type the name of the variable as it appears in the upper right
# click the blue button next to the name data.df and it will open up the names
# note it is case sensitive
x_variable_regression = "stream_width_cm" # has to be a number - not a chr variable
y_variable_regression = "terr_inv_shannon_diversity" # has to be a number - not a chr variable
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # RUN THE CODE BELOW TO SEE YOUR PLOT # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# construct the formula dynamically
regression_formula <- as.formula(paste(y_variable_regression, "~", x_variable_regression))

# run the commands below-------
# this will run your regression and save the figure just like you did in class
# Run the Regression model for Aquatic Diversity versus Plant Richness -------
regressison.model <- lm(regression_formula, data = data.df)
# Show the summary of the regression model
summary(regressison.model)

# Extracting coefficients - the slope and intercept
linear_coefficients <- coef(regressison.model)
intercept <- linear_coefficients[1]
slope <- linear_coefficients[2]

# Create the line equation as a string
regression_line_equation <- sprintf("y = %.4fx + %.4f", slope, intercept)

# Print the line equation for aquatic diveristy versus plant richness
print(regression_line_equation)

# Create the Aquatic Diversity verse plant richness plot -------
regression.plot <- data.df |> 
  ggplot(aes(x = !!sym(x_variable_regression), y = !!sym(y_variable_regression))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = x_variable_regression, y = y_variable_regression) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  annotate("text", 
           x = min(data.df[[x_variable_regression]], na.rm = TRUE) + 0.05,  # Adjust for position ------
           y = max(data.df[[y_variable_regression]], na.rm = TRUE) * 0.8,  # Adjust for position -------
           label = paste("Linear Regression Equation: y =", 
                         round(coef(regressison.model)[2], 2), "x +", 
                         round(coef(regressison.model)[1], 2), "\n", 
                         "r^2 =", round(summary(regressison.model)$r.squared, 2), "\n", 
                         "p =", round(summary(regressison.model)$coefficients[2, 4], 4)),
           size = 4, hjust = 0)

# Now show the Regression plot ------
regression.plot


# Save the  regression plot-------
ggsave(regression.plot, file = "figures/regression_plot.pdf", 
       width = 6, height = 6, units = "in", dpi = 300)

