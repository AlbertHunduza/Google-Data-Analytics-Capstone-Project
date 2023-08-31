# Load the required library
library(dplyr)
library(ggplot2)
library(corrplot)


# Importing relevant datasets
data_folder_path <- "Fitabase Data 4.12.16-5.12.16/" # sets path of folder containing files

daily_activity <- read.csv(paste(data_folder_path,"dailyActivity_merged.csv",sep=""))
sleep_day <- read.csv(paste(data_folder_path,"sleepDay_merged.csv",sep=""))
weight_log <- read.csv(paste(data_folder_path,"weightLogInfo_merged.csv",sep=""))
hourly_intensities <- read.csv(paste(data_folder_path,"hourlyIntensities_merged.csv",sep=""))
hourly_steps <- read.csv(paste(data_folder_path,"hourlySteps_merged.csv",sep=""))
heart_rate <- read.csv(paste(data_folder_path,"heartrate_seconds_merged.csv",sep=""))

# Checking for NaNs in each dataset
n_nans_daily_activity <- colSums(is.na(daily_activity))
n_nans_sleep_day <- colSums(is.na(sleep_day))
n_nans_weight_log <- colSums(is.na(weight_log))
n_nans_hourly_intensities <- colSums(is.na(hourly_intensities))
n_nans_hourly_steps <- colSums(is.na(hourly_steps))
n_nans_heart_rate <- colSums(is.na(heart_rate))

# Print the count of NaNs for each dataset
n_nans_daily_activity
n_nans_sleep_day
n_nans_weight_log
n_nans_hourly_intensities
n_nans_hourly_steps
n_nans_heart_rate

# Replace NaNs in the "Fat" column with "unreported"
weight_log$Fat <- ifelse(is.na(weight_log$Fat), "unreported", weight_log$Fat)

# checking if NaNs have been dropped
n_nans_weight_log <- colSums(is.na(weight_log))
n_nans_weight_log

# Checking for duplicate rows in each dataset
n_duplicates_daily_activity <- sum(duplicated(daily_activity))
n_duplicates_sleep_day <- sum(duplicated(sleep_day))
n_duplicates_weight_log <- sum(duplicated(weight_log))
n_duplicates_hourly_intensities <- sum(duplicated(hourly_intensities))
n_duplicates_hourly_steps <- sum(duplicated(hourly_steps))
n_duplicates_heart_rate <- sum(duplicated(heart_rate))

# Print the count of duplicate rows for each dataset
n_duplicates_daily_activity
n_duplicates_sleep_day
n_duplicates_weight_log
n_duplicates_hourly_intensities
n_duplicates_hourly_steps
n_duplicates_heart_rate

# Remove duplicate rows from the sleep_day dataframe
sleep_day_cleaned <- sleep_day[!duplicated(sleep_day), ]

# check if duplicates have been removed
n_duplicates_sleep_day_cleaned <- sum(duplicated(sleep_day_cleaned))
n_duplicates_sleep_day_cleaned

# merging the hourly_intensities and hourly_steps dataframes
hourly_exercises <- merge(hourly_intensities, hourly_steps, by = c("Id", "ActivityHour"))

# analyzing the daily_activity dataframe

# Get the number of unique IDs/people in the "Id" column
unique_id_count <- daily_activity %>%
  distinct(Id) %>%
  nrow()

# Print the number of unique IDs/people
print(unique_id_count)

# Convert the "ActivityDate" column to Date format
daily_activity %>%
  mutate(ActivityDate = as.Date(ActivityDate, format = "%m/%d/%Y")) %>%
  distinct(ActivityDate) %>%
  summarise(unique_date_count = n())



# Convert the "ActivityDate" column to Date format and group by Id
result <- daily_activity %>%
  mutate(ActivityDate = as.Date(ActivityDate, format = "%m/%d/%Y")) %>%
  group_by(Id) %>%
  summarise(unique_date_count = n_distinct(ActivityDate))

# Set the option to display all rows
options(dplyr.print_max = Inf)

# Print the result
print(result)

# Calculate the average number of steps and distance per person
average_per_person <- daily_activity %>%
  group_by(Id) %>%
  summarise(average_steps = mean(TotalSteps, na.rm = TRUE),
            average_distance = mean(TotalDistance, na.rm = TRUE))

# Print the result
print(average_per_person)

# Calculate the average number of steps and distance per person
average_per_person <- daily_activity %>%
  group_by(Id) %>%
  summarise(average_steps = mean(TotalSteps, na.rm = TRUE),
            average_distance = mean(TotalDistance, na.rm = TRUE))

# Create a histogram for average steps
ggplot(average_per_person, aes(x = average_steps)) +
  geom_histogram(binwidth = 500) +
  labs(title = "Histogram of Average Steps per Person",
       x = "Average Steps",
       y = "Frequency")

# Create a histogram for average distance
ggplot(average_per_person, aes(x = average_distance)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of Average Distance per Person",
       x = "Average Distance",
       y = "Frequency")

# Select the relevant columns
selected_columns <- daily_activity %>%
  select(TotalSteps, TotalDistance, Calories)

# Calculate the correlation matrix
correlation_matrix <- cor(selected_columns, use = "complete.obs")

# Visualize the correlation matrix with a title
corrplot(correlation_matrix, method = "shade", type = "upper",
         tl.col = "black", tl.srt = 45)


# Convert the "ActivityDate" column to Date format and add a new column for the day of the week
daily_activity_with_day <- daily_activity %>%
  mutate(ActivityDate = as.Date(ActivityDate, format = "%m/%d/%Y"),
         DayOfWeek = weekdays(ActivityDate))

# Group by day of the week and calculate the average distance
average_distance_per_day <- daily_activity_with_day %>%
  group_by(DayOfWeek) %>%
  summarise(average_distance = mean(TotalDistance, na.rm = TRUE))

# Order the days of the week
day_order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
average_distance_per_day$DayOfWeek <- factor(average_distance_per_day$DayOfWeek, levels = day_order)

# Create a bar plot
ggplot(average_distance_per_day, aes(x = DayOfWeek, y = average_distance, fill = DayOfWeek)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Distance per Day of the Week",
       x = "Day of the Week",
       y = "Average Distance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Select the relevant columns
selected_columns <- daily_activity %>%
  select(VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance, Calories)

# Calculate the correlation matrix
correlation_matrix <- cor(selected_columns, use = "complete.obs")

# Visualize the correlation matrix with a title
corrplot(correlation_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45)

# Select the relevant columns
selected_columns <- daily_activity %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories)

# Calculate the correlation matrix
correlation_matrix <- cor(selected_columns, use = "complete.obs")

# Visualize the correlation matrix with a title
corrplot(correlation_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45)
