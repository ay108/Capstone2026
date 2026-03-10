#Week 8 HW
library(tidyverse)
library(sinaplot)

df <- read.csv("/Users/ashleyyang/Desktop/DS Capstone/Week5/tmpwmdgd3ai.csv")
colnames(df)
View(df)
#only like 1% of violations are rodent violations
#get rid of na's 
df <- df %>%
  filter(!is.na(resultdttm))
#create flag that indicates whether the violation is related to rodents
df$rodent_violation_flag <- grepl("rodent", df$comments, ignore.case = TRUE)
table(df$rodent_violation_flag)
#count the number of inspections per day
#change the format to standard datetime to group by date
df$date <- as.Date(as.POSIXct(df$resultdttm, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

#for some reason, this regular version is not working...
# summary_by_date <- df %>%
#   group_by(date) %>%
#   summarize(
#     count_rodent = sum(rodent_violation_flag),
#     num_inspections = n_distinct(businessname)
#   )
summary_by_date <- df %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(
    count_rodent = sum(rodent_violation_flag, na.rm = TRUE),
    num_inspections = n_distinct(businessname),
    .groups = "drop"
  )

head(summary_by_date)
#drop the dates with NA's in them
summary_by_date <- drop_na(summary_by_date)
table(summary_by_date$num_inspections)
#needs to be data cleaning, this makes no sense; 1 inspection usually per day but then 
#all of a sudden, thousands?
summary_by_date %>%
  filter(num_inspections > 1000)

head(df[df$date=="2022-03-15",])
df %>%
  filter(date == "2012-12-30") %>%
  distinct(timestamp)
#it seems like those spikes are mass-uploads... they all have the same result timestamp

#create a rate variable 
summary_by_date$rodent_rate <- summary_by_date$count_rodent/ summary_by_date$num_inspections
tail(summary_by_date)
#plotting the num inspections because it seems a little fishy
sinaplot(summary_by_date$num_inspections, main="Sinaplot of number inspections per day", ylab="number of inspections")
sinaplot(summary_by_date$num_inspections, ylim = c(0, 10), main="Sinaplot of number inspections per day", ylab="number of inspections")
ggplot(summary_by_date, aes(x = date, y = num_inspections)) +
  geom_line() +
  labs(
    title = "Number of Restaurant Inspections Over Time",
    x = "Date",
    y = "Inspections Per Day"
  )

#weather data
weather <- read.csv("/Users/ashleyyang/Desktop/DS Capstone/boston_weather_data.csv")
weather <- weather %>%
  mutate(month = month(time))