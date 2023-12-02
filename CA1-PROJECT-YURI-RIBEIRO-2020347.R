# Read the dataset 
crimes_Boston <- read.csv("crime.csv")
offense_codes <- read.csv("offense_codes.csv")

# check the datasets
print(head(crimes_Boston))
print(head(offense_codes))

library(dplyr)

# merge both datasets and drop the column CODE from offense_codes
crimes_complete <- left_join(crimes_Boston, offense_codes %>% select(CODE, NAME), by = c("OFFENSE_CODE" = "CODE"))
head(crimes_complete)


# With the library skimr we can have a complete data summary with the mean/median, minimum, maximum and standard deviation
install.packages("skimr")
library(skimr)
numerical_summary_skim <- skim(crimes_complete)

print(numerical_summary_skim)




# ------------------ Preparation of the data ---------------------------
library(stringi)
# 
crimes_complete$OFFENSE_DESCRIPTION <- stri_trans_general(crimes_complete$OFFENSE_DESCRIPTION, "Latin-ASCII")
crimes_complete$DAY_OF_WEEK <- stri_trans_general(crimes_complete$DAY_OF_WEEK, "Latin-ASCII")


# Replace 'Y' with 'YES' and null with 'NO' in the 'SHOOTING' column
crimes_complete <- crimes_complete %>%
  mutate(SHOOTING = ifelse(coalesce(SHOOTING, "") == "", 'NO', ifelse(SHOOTING == 'y', 'YES', SHOOTING)))

# Replace "(0.00000000, 0.00000000)" with null in the 'Location' column
crimes_complete <- crimes_complete %>%
  mutate(Location = ifelse(Location == "(0.00000000, 0.00000000)", NA, Location))


# Calculate the percentage of missing values in each column
missing_percentage <- colMeans(is.na(crimes_complete)) * 100

# Print the result
cat("Missing values (%):\n")
print(missing_percentage)

# Data type of variables and bar plot
library(ggplot2)
var_types <- sapply(crimes_complete, class)
plot_data_types <- data.frame(variable = names(var_types), type = var_types)

ggplot(plot_data_types, aes(x = type, fill = type))+
  geom_bar()+
  labs(title = "Distribution of Variables types",
       x= "Variable type",
       y = "Count") +
  theme_minimal()

# Min-MAX normalization, Z-score Standadization 
columns_to_apply <- c("OFFENSE_CODE", "REPORTING_AREA", "YEAR", "MONTH")

# Min-Max
crimes_complete_minMax <- crimes_complete %>%
  mutate(across(all_of(columns_to_apply), ~ (.-min(.))/(max(.)-min(.))))
print(select(crimes_complete_minMax, all_of(columns_to_apply)))                

#Z-Score
crimes_complete_zScore <- crimes_complete %>%
  mutate(across(all_of(columns_to_apply), scale))
print(select(crimes_complete_zScore, all_of(columns_to_apply)))    


# Heatmap 
# First we create a data frame to combine the two variables ( Offense Description and Days of Week) selecting 5 offenses

selected_offenses <- c("BURGLARY - RESIDENTIAL - NO FORCE", "HARASSMENT", "WARRANT ARREST", "VERBAL DISPUTE", "VANDALISM")


# create the data frame for the heatmap
heatmap_dataFrame <- crimes_complete %>%
  filter(OFFENSE_DESCRIPTION %in% selected_offenses) %>%
  group_by(OFFENSE_DESCRIPTION, DAY_OF_WEEK) %>%
  summarise(count = n_distinct(INCIDENT_NUMBER)) %>%
  ungroup()

# Create a heatmap using ggplot2
heatmap_plot <- ggplot(heatmap_dataFrame, aes(x = DAY_OF_WEEK, y = OFFENSE_DESCRIPTION, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(title = "Heatmap of Selected Offenses vs. Days of Week",
       x = "Days of Week",
       y = "Offense Description") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Display the heatmap
plot(heatmap_plot)



#  Dummy coding to the categorical OFFENSE_DESCRIPTION column
dummy_coding <- model.matrix(~ OFFENSE_DESCRIPTION - 1, data = crimes_complete)

# Combine the dummy-coded variables with the original data
crimes_complete_dummy <- cbind(crimes_complete, dummy_coding)

# View the resulting data

crimes_complete_dumy

