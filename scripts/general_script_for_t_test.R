# this file is set up to be able to run a T TEST by entering the data in lines 19 and 20

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
x_variable_ttest = "restoration_status" # has to be a number - not a chr variable
y_variable_ttest = "terr_inv_shannon_diversity" # has to be a number - not a chr variable

# Filter out NA values in x and y variables
filtered_data.df <- data.df |> 
  filter(!is.na(!!sym(x_variable_ttest)) & !is.na(!!sym(y_variable_ttest)))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # RUN THE CODE BELOW TO SEE YOUR PLOT # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# T-TEST---------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# T-Test for Aquatic Diversity in Restored and Unrestored Sites---------

# construct the formula dynamically
t_test_formula <- as.formula(paste(y_variable_ttest, "~", x_variable_ttest))


t_test.model <- t.test(t_test_formula, 
                       data = filtered_data.df,
                               var.equal = FALSE)

# what does the T-Test tell us - will show at the bottom - 
# We need to record the T Value, the P Value and the Degrees of Freedom
t_test.model

# Extract the t-test results to use in the graph-----
t_stat <- round(t_test.model$statistic, 2)  # t-statistic
p_value <- round(t_test.model$p.value, 4)   # p-value
df <- round(t_test.model$parameter, 2)      # degrees of freedom

# Make the Aquatic T Test Plot ------
t_test.plot <- filtered_data.df |> 
  ggplot(aes(x = !!sym(x_variable_ttest), y = !!sym(y_variable_ttest))) +
  stat_summary(fun=mean, na.rm=TRUE,  geom = "bar",
               fill="white", colour="black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2) +
  labs(x = x_variable_ttest, y = y_variable_ttest)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  annotate("text", x = 1, 
           y = max(data.df[[y_variable_ttest]], na.rm = TRUE)  * .8, # change the 0.8 to a value that allow syou to plot it
           label = paste("t =", t_stat, "\n", "p =", p_value, "\n", "df =", df),
           size = 5, hjust = 0.5)

# Show the Aquatic T Test Plot
t_test.plot

# Save the Aquatic T Test Plot-------
ggsave(t_test.plot, file = "figures/t_test_plot.pdf", 
       width = 6, height = 6, units = "in", dpi = 300)

