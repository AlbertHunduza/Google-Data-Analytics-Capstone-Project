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



