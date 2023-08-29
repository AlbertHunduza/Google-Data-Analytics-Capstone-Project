# Importing relevant datasets
init_path <- "Fitabase Data 4.12.16-5.12.16/" # sets path of folder containing files

daily_activity <- read.csv(paste(init_path,"dailyActivity_merged.csv",sep=""))
hourly_intensities <- read.csv(paste(init_path,"hourlyIntensities_merged.csv",sep=""))
hourly_steps <- read.csv(paste(init_path,"hourlySteps_merged.csv",sep=""))
sleep_day <- read.csv(paste(init_path,"sleepDay_merged.csv",sep=""))
weight_log <- read.csv(paste(init_path,"weightLogInfo_merged.csv",sep=""))
heart_rate <- read.csv(paste(init_path,"heartrate_seconds_merged.csv",sep=""))

View(daily_activity)
head(daily_activity)
