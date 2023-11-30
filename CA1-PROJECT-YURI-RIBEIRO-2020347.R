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

crimes_


# ------------------ Preparation of the data ---------------------------

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
