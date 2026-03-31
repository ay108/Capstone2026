#Week 8 HW
library(tidyverse)
library(sinaplot)
library(runner)
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
  distinct(resultdttm)

#plotting the num inspections because it seems a little fishy
sinaplot(summary_by_date$num_inspections, main="Sinaplot of number inspections per day", ylab="number of inspections")
sinaplot(summary_by_date$num_inspections, ylim = c(0, 10), main="Sinaplot of number inspections per day", ylab="number of inspections")
plot(summary_by_date$date, summary_by_date$num_inspections,type = "l",xlab = "Date",
  ylab = "Inspections Per Day",
  main = "Number of Restaurant Inspections Over Time")

#it seems like those spikes are mass-uploads... they all have the same result timestamp
#I want to find outliers and remove them...
Q1 <- quantile(summary_by_date$num_inspections, 0.25)
Q3 <- quantile(summary_by_date$num_inspections, 0.75)
IQR_val <- Q3 - Q1

# Define outliers as > Q3 + 1.5*IQR
outlier_threshold <- Q3 + 1.5 * IQR_val

outliers <- summary_by_date %>%
  filter(num_inspections > outlier_threshold)
# View outlier dates
outliers
#remove outliers?
####INQUIRY? should i log this? 
#create a rate variable
summary_by_date$rodent_rate <- summary_by_date$count_rodent/ summary_by_date$num_inspections
tail(summary_by_date)
summary_by_date %>%
  filter(num_inspections <= outlier_threshold)
hist(summary_by_date$num_inspections, main="Histogram of cleaned number of inspections", xlab= "number of inspections")
hist(summary_by_date$rodent_rate, main="Histogram of cleaned number of inspections", xlab= "number of inspections")
#weather data
weather <- read.csv("/Users/ashleyyang/Desktop/DS Capstone/boston_weather_data.csv")
weather <- weather %>%
  mutate(month = month(time))
weather$summer <- as.numeric(weather$month %in% c(6,7,8))
head(weather)
weather[is.na(weather$tmax),]
class(weather$time)
weather$time<-as.Date(weather$time)
#wanna plot the temperatures over time
weather_summer<- filter(weather, weather$summer ==1)
plot(weather_summer$time,weather_summer$tmax)
plot(weather_summer$time,weather_summer$tmax, ylim = c(30,40))
#seems like a very weak increase
# plot(weather$time,weather$tmax)
#want to mark dates with whether they were an extreme temp (85 for 3 days or longer)
weather$hot_day <- weather$tmax >= 29.44
r<-rle(weather$hot_day)
class(r)
r
extreme_runs <- r$values & r$lengths >= 3
#this means to replicate extreme run values across lengths (False replicated 61 times)
weather$extreme_heat <- rep(extreme_runs, r$lengths)
weather$extreme_heat <- as.numeric(weather$extreme_heat)
#wanna plot this extreme heat 
#barplot of 1's in extreme_heat for each year
weather <- weather %>%
  mutate(year = year(time))
#need to group by to find the number of heat advisories per year 
#count the number of advisories per year (continuous ... more than 3 =1)

weather$heat_wave_start <- weather$extreme_heat & !c(FALSE, head(weather$extreme_heat, -1))
weather_summary<-aggregate(heat_wave_start ~ year, data = weather, FUN = sum)
barplot(heat_wave_start ~ year, data = weather_summary,
        main = "Heat Advisory Warnings Over the Years",
        xlab = "Year", ylab = "Number of Heat Advisories")
#it looks a little stable/uniform... don't know if this question
#will get at anything...

#need to join with the food data
full<-left_join(summary_by_date, weather, by = c("date"="time"))
full<-full[,c("date", "count_rodent", "num_inspections", "rodent_rate", "tmin","tmax", "temp","month", "summer", "hot_day","extreme_heat","year","heat_wave_start")]

#now want to plot the rodent_rate and extreme heats(alr have time embedded; maybe have labels with the year?)

#but first, wanna model this...
#i want rodent rate and date and 
heatmap(full[,2:13])
full$hot_day<-as.numeric(full$hot_day)
full$heat_wave_start<-as.numeric(full$heat_wave_start)
#now want to have the count per year, and also the rodent rate per year(averaged?)
weather_summary$avg_rodent_rate <- aggregate(rodent_rate ~ year, data = full, FUN = mean)$rodent_rate
weather_summary<- weather_summary[,c('year', 'avg_rodent_rate', 'heat_wave_start')]
#I see that there is missing data for 2026 and 2006; I removed those years
weather_summary_cleaned<- weather_summary[2:20,]
#plotting them against each other
par(mfrow = c(2, 1))
plot(weather_summary_cleaned$year, weather_summary_cleaned$heat_wave_start, 
     type = "b", col = "tomato", 
     xlab = "Year", ylab = "Heat Advisories", main = "Heat Advisories Per Year")

plot(weather_summary_cleaned$year, weather_summary_cleaned$avg_rodent_rate, 
     type = "b", col = "steelblue", 
     xlab = "Year", ylab = "Avg Rodent Rate", main = "Rodent Rate Per Year")
#I see that there is missing data for 2026 and 2006; I removed those years
#next, want a heatmap of variables now
head(full)
#wanna get rid of 2006 and 2026 in full too
full <- full[!full$year %in% c(2006, 2026), ]
full <- full[!is.na(full$year), ]
full_num <- full[,c("rodent_rate", "tmax", "tmin", "temp", "month", "summer", "hot_day", "extreme_heat", "year", "heat_wave_start")]
library(corrplot)
par(mfrow = c(1, 1))
cor_data <- na.omit(full_num)
cor_matrix <- cor(full_num)
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black",
         title = "Correlation with Rodent Rate",
         mar = c(0,0,1,0), )
#really need to add a lag to explore if it is truly not correlated...
#now want to do a mixed effects model
library(lme4)


model <- lmer(
  rodent_rate ~ tmax + summer + hot_day + extreme_heat +
    (1 | year) + (1 | month),
  data = full
)
library(car)
vif(lm(rodent_rate ~ temp + tmin + tmax + summer + hot_day + extreme_heat, data = full))
vif(lm(rodent_rate ~ tmax + summer + hot_day + extreme_heat, data = full))#this is fine now!


