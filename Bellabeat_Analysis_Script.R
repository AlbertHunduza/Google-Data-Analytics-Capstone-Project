# Load the required library
library(dplyr)
library(ggplot2)
library(corrplot)
library(gridExtra)



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

# Analyzing the sleep_day (sleep_day_cleaned) dataframe

# Number of unique people (Ids)
unique_people_count <- sleep_day_cleaned %>%
  summarise(unique_people = n_distinct(Id))

# Print the number of unique people
print(unique_people_count)

# Number of unique days each person has a record for
unique_days_per_person <- sleep_day_cleaned %>%
  group_by(Id) %>%
  summarise(unique_days = n_distinct(as.Date(SleepDay, format = "%m/%d/%Y")))

# Print the number of unique days per person
print(unique_days_per_person)



# Calculate the percentages
percentage_data <- sleep_day_cleaned %>%
  group_by(TotalSleepRecords) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / sum(count)) * 100)

# Create a bar graph with percentages
ggplot(percentage_data, aes(x = factor(TotalSleepRecords), y = percentage)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Distribution of TotalSleepRecords",
       x = "Total Sleep Records",
       y = "Percentage") +
  theme_minimal() +
  scale_x_discrete(labels = function(x) sprintf("%d", as.integer(x))) 

# Calculate the average TotalSleepRecords per unique Id
average_sleep_records_per_id <- sleep_day_cleaned %>%
  group_by(Id) %>%
  summarise(average_sleep_records = mean(TotalSleepRecords, na.rm = TRUE))

# Print the result
print(average_sleep_records_per_id)

# Create a boxplot for TotalMinutesAsleep
ggplot(sleep_day_cleaned, aes(y = TotalMinutesAsleep)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Boxplot of TotalMinutesAsleep",
       y = "Total Minutes Asleep") +
  theme_minimal()

# Calculate the average TotalMinutesAsleep
average_minutes_asleep <- mean(sleep_day_cleaned$TotalMinutesAsleep, na.rm = TRUE)

# Print the result
print(average_minutes_asleep)

# Calculate the average TotalMinutesAsleep per person
average_minutes_asleep_per_person <- sleep_day_cleaned %>%
  group_by(Id) %>%
  summarise(average_minutes_asleep = mean(TotalMinutesAsleep, na.rm = TRUE))

# Print the result
print(average_minutes_asleep_per_person)

# Calculate the percentage of time spent in bed that each person was actually asleep
percentage_time_asleep_per_person <- sleep_day_cleaned %>%
  mutate(PercentageAsleep = (TotalMinutesAsleep / TotalTimeInBed) * 100) %>%
  group_by(Id) %>%
  summarise(AveragePercentageAsleep = mean(PercentageAsleep, na.rm = TRUE))

# Print the result
print(percentage_time_asleep_per_person)

# Convert the "SleepDay" column to Date format and add a new column for the day of the week
sleep_day_cleaned <- sleep_day_cleaned %>%
  mutate(SleepDay = as.Date(SleepDay, format = "%m/%d/%Y"),
         DayOfWeek = weekdays(SleepDay))

# Calculate the average TotalMinutesAsleep and percentage of Time in Bed spent asleep for each day of the week
average_sleep_per_day_of_week <- sleep_day_cleaned %>%
  group_by(DayOfWeek) %>%
  summarise(AverageTotalMinutesAsleep = mean(TotalMinutesAsleep, na.rm = TRUE),
            AveragePercentageAsleep = mean((TotalMinutesAsleep / TotalTimeInBed) * 100, na.rm = TRUE))

# Create bar graphs
ggplot(average_sleep_per_day_of_week, aes(x = DayOfWeek)) +
  geom_bar(aes(y = AverageTotalMinutesAsleep), stat = "identity", fill = "blue", alpha = 0.6, position = "dodge") +
  geom_bar(aes(y = AveragePercentageAsleep), stat = "identity", fill = "green", alpha = 0.6, position = "dodge") +
  labs(title = "Average Total Minutes Asleep and Percentage of Time in Bed Spent Asleep by Day of the Week",
       x = "Day of the Week",
       y = "Value") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Percentage of Time in Bed Spent Asleep")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("blue", "green")) +
  scale_alpha_manual(values = c(0.6, 0.6)) +
  guides(fill = guide_legend(title = "Legend", override.aes = list(alpha = 1)))

# Analyzing weight_log dataframe

# Number of unique people (Ids)
unique_people_count <- weight_log %>%
  summarise(unique_people = n_distinct(Id))

# Print the number of unique people
print(unique_people_count)

# Total days with records for each person
total_days_per_person <- weight_log %>%
  group_by(Id) %>%
  summarise(total_days = n_distinct(Date))

# Print the total days with records per person
print(total_days_per_person)

# Convert the "Date" column to a proper date format
weight_log$Date <- as.POSIXct(weight_log$Date, format = "%m/%d/%Y %I:%M:%S %p")

# Find the starting weight for each person
starting_weight <- weight_log %>%
  group_by(Id) %>%
  summarise(starting_weight = first(WeightKg))

# Find the ending weight for each person
ending_weight <- weight_log %>%
  group_by(Id) %>%
  summarise(ending_weight = last(WeightKg))

# Combine the starting and ending weights
weight_summary <- left_join(starting_weight, ending_weight, by = "Id")

# Create a bar graph
ggplot(weight_summary, aes(x = factor(Id))) +
  geom_bar(aes(y = starting_weight), stat = "identity", fill = "blue", alpha = 0.6, position = position_dodge(width = 0.8)) +
  geom_bar(aes(y = ending_weight), stat = "identity", fill = "green", alpha = 0.6, position = position_dodge(width = 0.8)) +
  labs(title = "Starting and Ending Weights for Each Person",
       x = "ID",
       y = "Weight (Kg)") +
  scale_fill_manual(values = c("blue", "green")) +
  scale_alpha_manual(values = c(0.6, 0.6)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Convert the "Date" column to a proper date format
weight_log$Date <- as.POSIXct(weight_log$Date, format = "%m/%d/%Y %I:%M:%S %p")

# Create a function to categorize BMI
categorize_bmi <- function(bmi) {
  if (bmi < 18.5) {
    return("Underweight")
  } else if (bmi >= 18.5 && bmi <= 25) {
    return("Normal")
  } else {
    return("Overweight")
  }
}

# Categorize BMI for the first and last records
weight_log_summary <- weight_log %>%
  group_by(Id) %>%
  arrange(Date) %>%
  summarise(StartCategory = categorize_bmi(first(BMI)),
            EndCategory = categorize_bmi(last(BMI)))

# Create pie charts for the start and end categories
pie_chart_start <- weight_log_summary %>%
  count(StartCategory) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = n, fill = StartCategory, label = paste0(percentage, "%"))) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("red", "green", "blue")) +  # Red for "Overweight," green for "Normal"
  labs(title = "Distribution of BMI Categories at Start") +
  theme_void() +
  theme(legend.position = "right")

pie_chart_end <- weight_log_summary %>%
  count(EndCategory) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = n, fill = EndCategory, label = paste0(percentage, "%"))) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("red", "green", "blue")) +  # Red for "Overweight," green for "Normal"
  labs(title = "Distribution of BMI Categories at End") +
  theme_void() +
  theme(legend.position = "right")

# Display the pie charts
gridExtra::grid.arrange(pie_chart_start, pie_chart_end, ncol = 2)

# Number of unique people (Ids)
unique_people_count <- hourly_exercises %>%
  summarise(unique_people = n_distinct(Id))

# Print the number of unique people
print(unique_people_count)

# Number of days and unique hours per person
days_and_hours_per_person <- hourly_exercises %>%
  group_by(Id) %>%
  summarise(total_days = n_distinct(as.Date(ActivityHour)),
            unique_hours = n_distinct(ActivityHour))

# Print the number of days and unique hours per person
print(days_and_hours_per_person)

# Calculate the average TotalIntensity and average StepTotal per person
average_intensity_and_steps_per_person <- hourly_exercises %>%
  group_by(Id) %>%
  summarise(average_total_intensity = mean(TotalIntensity, na.rm = TRUE),
            average_step_total = mean(StepTotal, na.rm = TRUE))

# Print the result
print(average_intensity_and_steps_per_person)

# Create a histogram for average TotalIntensity
histogram_intensity <- ggplot(average_intensity_and_steps_per_person, aes(x = average_total_intensity)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.6) +
  labs(title = "Histogram of Average TotalIntensity per Person",
       x = "Average TotalIntensity",
       y = "Frequency") +
  theme_minimal()

# Calculate the average TotalIntensity and average StepTotal per person
average_intensity_and_steps_per_person <- hourly_exercises %>%
  group_by(Id) %>%
  summarise(average_total_intensity = mean(TotalIntensity, na.rm = TRUE),
            average_step_total = mean(StepTotal, na.rm = TRUE))

# Create a histogram for average TotalIntensity
histogram_intensity <- ggplot(average_intensity_and_steps_per_person, aes(x = average_total_intensity)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.6) +
  labs(title = "Histogram of Average TotalIntensity per Person",
       x = "Average TotalIntensity",
       y = "Frequency") +
  theme_minimal()

# Create a histogram for average StepTotal
histogram_steps <- ggplot(average_intensity_and_steps_per_person, aes(x = average_step_total)) +
  geom_histogram(binwidth = 1000, fill = "green", alpha = 0.6) +
  labs(title = "Histogram of Average StepTotal per Person",
       x = "Average StepTotal",
       y = "Frequency") +
  theme_minimal()

# Display the histograms
gridExtra::grid.arrange(histogram_intensity, histogram_steps, ncol = 2)

# Convert the "ActivityHour" column to a proper date format and extract the day of the week
hourly_exercises <- hourly_exercises %>%
  mutate(ActivityHour = as.POSIXct(ActivityHour, format = "%m/%d/%Y %I:%M:%S %p"),
         DayOfWeek = weekdays(ActivityHour))

# Calculate the average TotalIntensity and average StepTotal per day of the week
average_intensity_per_day <- hourly_exercises %>%
  group_by(DayOfWeek) %>%
  summarise(average_total_intensity = mean(TotalIntensity, na.rm = TRUE))

average_steps_per_day <- hourly_exercises %>%
  group_by(DayOfWeek) %>%
  summarise(average_step_total = mean(StepTotal, na.rm = TRUE))

# Create a bar plot for average TotalIntensity per day of the week
bar_plot_intensity <- ggplot(average_intensity_per_day, aes(x = DayOfWeek, y = average_total_intensity)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.6) +
  labs(title = "Average TotalIntensity per Day of the Week",
       x = "Day of the Week",
       y = "Average TotalIntensity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a bar plot for average StepTotal per day of the week
bar_plot_steps <- ggplot(average_steps_per_day, aes(x = DayOfWeek, y = average_step_total)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.6) +
  labs(title = "Average StepTotal per Day of the Week",
       x = "Day of the Week",
       y = "Average StepTotal") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the bar plots
gridExtra::grid.arrange(bar_plot_intensity, bar_plot_steps, ncol = 2)


#Analyzing the heart_rate dataframe
# Number of unique people (Ids)
unique_people_count <- heart_rate %>%
  summarise(unique_people = n_distinct(Id))

# Print the number of unique people
print(unique_people_count)

# Number of unique records per person
unique_records_per_person <- heart_rate %>%
  group_by(Id) %>%
  summarise(unique_records = n_distinct(Time))

# Print the number of unique records per person
print(unique_records_per_person)

# Calculate the average heart rate per person
average_heart_rate_per_person <- heart_rate %>%
  group_by(Id) %>%
  summarise(average_heart_rate = mean(Value, na.rm = TRUE))

# Create a bar plot for average heart rate per person
bar_plot_heart_rate <- ggplot(average_heart_rate_per_person, aes(x = factor(Id), y = average_heart_rate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.6) +
  labs(title = "Average Heart Rate per Person",
       x = "ID",
       y = "Average Heart Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the bar plot
print(bar_plot_heart_rate)



# Convert the "Time" column to a proper datetime format
heart_rate$Time <- as.POSIXct(heart_rate$Time, format = "%m/%d/%Y %I:%M:%S %p")

# Extract the hour of the day from the "Time" column
heart_rate$HourOfDay <- format(heart_rate$Time, format = "%H")

# Calculate the average heart rate for each hour of the day
average_heart_rate_per_hour <- heart_rate %>%
  group_by(HourOfDay) %>%
  summarise(average_heart_rate = mean(Value, na.rm = TRUE))

# Create a line plot for average heart rate per hour of the day
line_plot_heart_rate <- ggplot(average_heart_rate_per_hour, aes(x = HourOfDay, y = average_heart_rate, group = 1)) +
  geom_line(color = "blue") +
  labs(title = "Average Heart Rate per Hour of the Day",
       x = "Hour of the Day",
       y = "Average Heart Rate") +
  theme_minimal()

# Display the line plot
print(line_plot_heart_rate)