# Read the dataset 
crimes_Boston <- read.csv("crime.csv")

# Select only the first 7000 rows
crimes_Boston_subset <- head(crimes_Boston, 7000)

print(names(crimes_Boston_subset))

#Select only 10 columns to explore in this project
selected_columns <- c("INCIDENT_NUMBER", "OFFENSE_CODE","OFFENSE_CODE_GROUP", "OFFENSE_DESCRIPTION", "DISTRICT",
                      "YEAR", "MONTH", "DAY_OF_WEEK", "HOUR", "STREET")
crimes_Boston_subset <- crimes_Boston_subset[, selected_columns]

library(ggplot2)

# Histogram for Years (continuous variable)
ggplot(crimes_Boston_subset, aes(x = YEAR)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of YEAR")

# Bar plot for day of the week ( categorical variable)
ggplot(crimes_Boston_subset, aes(x = DAY_OF_WEEK)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Bar Plot of DAY_OF_WEEK")

# Count plot for Months (discrete variables)
ggplot(crimes_Boston_subset, aes(x = factor(MONTH))) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Count Plot of MONTH")
