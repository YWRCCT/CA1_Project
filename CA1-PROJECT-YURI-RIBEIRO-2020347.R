library(dplyr)
library(gridExtra)
library(stringi)
library(skimr)
library(ggplot2)


# Read the dataset 
crimes_Boston <- read.csv("crime.csv")
offense_codes <- read.csv("offense_codes.csv")

# check the datasets
print(head(crimes_Boston))
print(head(offense_codes))



# merge both datasets and drop the column CODE from offense_codes
crimes_complete <- left_join(crimes_Boston, offense_codes %>% select(CODE, NAME), by = c("OFFENSE_CODE" = "CODE"))
head(crimes_complete)


# With the library skimr we can have a complete data summary with the mean/median, minimum, maximum and standard deviation
install.packages("skimr")

numerical_summary_skim <- skim(crimes_complete)

print(numerical_summary_skim)




# ------------------ Preparation of the data ---------------------------

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

print(crimes_complete_dummy)


# Applying PCA to all numeric variables

# replace missing values with mean 
crimes_complete_v2 <- na.omit(crimes_complete)

# Extract the numeric columns for PCA
numeric_columns <- sapply(crimes_complete_v2, is.numeric)
data_for_pca <- crimes_complete_v2[, numeric_columns]
standardized_data <- scale(data_for_pca)


# PCA RESULT
pca_result <- prcomp(standardized_data, scale. = TRUE)

# Summary of PCA
summary(pca_result)

# PCA plot
plot(pca_result, type = "l", main = "PCA Plot")


# EXPLORATION

# Create a horizontal bar plot - Which district has the most/least number of reported crimes?

ggplot(crimes_complete, aes(x = reorder(DISTRICT, -table(DISTRICT)[DISTRICT]), fill = DISTRICT)) +
  geom_bar(stat = "count") +
  labs(x = "Number of Reported Crimes", y = "District") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +  
  coord_flip() 


# Create a vertical bar plot - which hours has more number of reported crimes?
bar_plot_hour <- ggplot(crimes_complete, aes(x = HOUR)) +
  geom_bar(stat = "count", fill = "skyblue") +
  labs(x = "Hour of the Day", y = "Number of Reported Crimes") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 23, 1), labels = sprintf("%02d:00", seq(0, 23, 1)))

# Display the bar plot
print(bar_plot_hour)


# The Top 10 and bottom of Crimes:
#Create a temporary data frame of top 10
top10_crimes <- data.frame(
  OFFENSE_CODE_GROUP = names(table(crimes_complete$OFFENSE_CODE_GROUP)),
  Count_crimes = as.numeric(table(crimes_complete$OFFENSE_CODE_GROUP)))

# Order the data frame by Count_crimes
top10_crimes <- top10_crimes[order(-top10_crimes$Count_crimes), ]

# Top 10 Types of Crimes 
top_10_plot_horizontal <- ggplot(top10_crimes[1:10, ], 
                                 aes(x = Count_crimes, y = reorder(OFFENSE_CODE_GROUP, -Count_crimes))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Number of Reported Crimes", y = "") +
  ggtitle("Top 10 Types of Crimes") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5))  

# Bottom 10 Types of Crimes 
bottom_10_plot_horizontal <- ggplot(top10_crimes[(nrow(top10_crimes)-9):nrow(top10_crimes), ], 
                                    aes(x = Count_crimes, y = reorder(OFFENSE_CODE_GROUP, Count_crimes))) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(x = "Number of Reported Crimes", y = "") +
  ggtitle("Bottom 10 Types of Crimes") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5))  

# display the plots 
grid.arrange(top_10_plot_horizontal, bottom_10_plot_horizontal, ncol = 1)

