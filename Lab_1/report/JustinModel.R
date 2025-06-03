library(car)
library(tidyverse)
library(stargazer)
library(conflicted)
library(tidyverse)
library(knitr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(lubridate)

#  get data


#  Lyn's section

bike_demand <- read_csv(
  "C:/Users/Owner/Desktop/MIDS/271/summer_25_central-main/summer_25_central-main/Labs/Lab_1/data/SeoulBikeData.csv", 
  locale = locale(encoding="latin1")
)

names(bike_demand) <- c("date", "rented_bike_count", "hour", "temperature_c", "humidity",
                        "wind_speed_m_s", "visibility_10m", "dew_point_temp_c", 
                        "solar_radiation_mj_m2", "rainfall_mm", "snowfall_cm", 
                        "season", "holiday", "functioning_day")
head(bike_demand)

print(summary(bike_demand))
sum(is.na(bike_demand))


#  TIME AND DAY ANALYSIS
bike_demand %>% mutate()

#  TIME OF DAY

plot_bike_cnt1 <- bike_demand %>% 
  ggplot(aes(x = rented_bike_count)) +
  geom_bar(width = 2) +
  ggtitle("Distribution of rented bike count") + 
  xlab("rented bike count") +
  ylab("")

plot_bike_cnt2 <- bike_demand %>% 
  ggplot(aes(x = rented_bike_count)) +
  geom_boxplot() +
  ggtitle("Distribution of rented bike count") + 
  xlab("rented bike count") +
  ylab("")

grid.arrange(plot_bike_cnt1, plot_bike_cnt2, nrow = 1, ncol = 2)

p1 <- bike_demand %>%
  group_by(hour) %>%
  summarise(total_rented = sum(rented_bike_count, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = total_rented)) +
  geom_line() +
  labs(
    title = "Total Rented Bikes by Hour",
    x = "Hour of the Day",
    y = "Total Rented Bikes"
  )


#  SEASON
p2 <- bike_demand %>%
  group_by(season) %>%
  summarise(avg_rented = mean(rented_bike_count, na.rm = TRUE)) %>%
  ggplot(aes(x = season, y = avg_rented)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Rented Bikes by Season",
    x = "Season",
    y = "Average Rented Bikes"
  )
p2

#  winter is lowest by far, as expected

#### Holiday
p3 <- bike_demand %>%
  group_by(season, holiday) %>%
  summarise(avg_rented = mean(rented_bike_count, na.rm = TRUE)) %>%
  ggplot(aes(x = season, y = avg_rented, fill=holiday)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Rented Bikes by Season",
    x = "Season",
    y = "Average Rented Bikes"
  )
p3

#  difference in holiday by season not much different


#  Bikes seem to be rented either in the morning when most workdays start
#  or in the afternoon when most workdays are over.  WIll check if 
#  this is the cause by separating weekdays and weekends and see if the peaks 
#  still exist on the weekends
bike_demand$day <- weekdays(as.Date(bike_demand$date))
bike_demand$weekend <- 0
bike_demand[bike_demand$day %in% c("Saturday","Sunday"), "weekend"] <- 1
bike_demand_weekday <- bike_demand[bike_demand$weekend == 0,]
bike_demand_weekend <- bike_demand[bike_demand$weekend == 1,]

p1_1 <- bike_demand_weekday %>%
  group_by(hour) %>%
  summarise(total_rented = sum(rented_bike_count, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = total_rented)) +
  geom_line() +
  labs(
    title = "Total Rented Bikes by Hour Weekdays",
    x = "Hour of the Day",
    y = "Total Rented Bikes"
  )

p1_2 <- bike_demand_weekend %>%
  group_by(hour) %>%
  summarise(total_rented = sum(rented_bike_count, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = total_rented)) +
  geom_line() +
  labs(
    title = "Total Rented Bikes by Hour Weekends",
    x = "Hour of the Day",
    y = "Total Rented Bikes"
  )

grid.arrange(p1_1, p1_2, nrow = 1, ncol = 2)

#  weekends still have the peaks...why the peaks then?

#  DAY OF WEEK
p1_3 <- bike_demand %>%
  group_by(day) %>%
  summarise(avg_rented = mean(rented_bike_count, na.rm = TRUE)) %>%
  ggplot(aes(x = day, y = avg_rented)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Rented Bikes by Day of Week",
    x = "Day of Week",
    y = "Average Rented Bikes"
  )
p1_3

#  No real difference

#  MONTH
bike_demand$month <- month(as.Date(bike_demand$date))

p1_4 <- bike_demand %>%
  group_by(month) %>%
  summarise(avg_rented = mean(rented_bike_count, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = avg_rented)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Rented Bikes by Day of Week",
    x = "Day of Week",
    y = "Average Rented Bikes"
  )
p1_4

#  No real difference from season, spikes in June

#  TIME OF DAY AND SEASOn

p1_5 <- bike_demand[bike_demand$season == "Winter",] %>%
  group_by(hour) %>%
  summarise(total_rented = sum(rented_bike_count, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = total_rented)) +
  geom_line() +
  labs(
    title = "Total Rented Bikes by Hour (Winter)",
    x = "Hour of the Day",
    y = "Total Rented Bikes"
  )

p1_6 <- bike_demand[bike_demand$season == "Spring",] %>%
  group_by(hour) %>%
  summarise(total_rented = sum(rented_bike_count, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = total_rented)) +
  geom_line() +
  labs(
    title = "Total Rented Bikes by Hour (Spring)",
    x = "Hour of the Day",
    y = "Total Rented Bikes"
  )

p1_7 <- bike_demand[bike_demand$season == "Summer",] %>%
  group_by(hour) %>%
  summarise(total_rented = sum(rented_bike_count, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = total_rented)) +
  geom_line() +
  labs(
    title = "Total Rented Bikes by Hour (Summer)",
    x = "Hour of the Day",
    y = "Total Rented Bikes"
  )

p1_8 <- bike_demand[bike_demand$season == "Autumn",] %>%
  group_by(hour) %>%
  summarise(total_rented = sum(rented_bike_count, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = total_rented)) +
  geom_line() +
  labs(
    title = "Total Rented Bikes by Hour (Autumn)",
    x = "Hour of the Day",
    y = "Total Rented Bikes"
  )

grid.arrange(p1_5, p1_6, p1_7, p1_8, nrow = 2, ncol = 2)


#  FOR DAY AND TIME alone, season seems to matter and hour seems to matter
#  might have dummy variables for hourly spikes at 8 and 16

p4 <- bike_demand %>% 
  ggplot(aes(x = temperature_c, y = rented_bike_count), color = factor(season)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Relationship Between Temperature and Bike Rentals",
    x = "Temperature (°C)",
    y = "Rented Bike Count"
  ) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  theme_minimal()

p5 <- bike_demand %>% 
  ggplot(aes(x = humidity, y = rented_bike_count)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Relationship Between Humidity and Bike Rentals",
    x = "Humidity",
    y = "Rented Bike Count"
  ) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  theme_minimal()
grid.arrange(p4, p5, nrow = 1, ncol = 2)

getAve <- function(){
  for (hour in 0:24){
    sum <- sum(bike_demand_weekend[(bike_demand_weekend$hour == hour), "rented_bike_count"]$rented_bike_count)
    len <- length(bike_demand_weekend[(bike_demand_weekend$hour == hour), "rented_bike_count"]$rented_bike_count)
    print(paste(hour,sum/len, sep=" : "))
  }
}

#  MOdeling
#  Assumptions
#  1.  count data = yes
#  2.  independence = yes (maybe day after holiday would have fewer?  assume yes for now)
#  3.  mean = variance = no
#  4.  


#  #  model_poisson_1 - Season, rainfall, and time of day will have the most signficant effects on bikes rented
#  #  model_poisson_2 - Include other weather terms, and indicators for time of day spikes (change hour to indicator var)
#  #  model_poisson_3 - Add interaction terms (temperature/season, time_of_day/weekend holiday)

#  variables
#  "hour", 
#  "temperature_c", 
#  "humidity",
#  "wind_speed_m_s",
#  "visibility_10m", 
#  "dew_point_temp_c", 
#  "solar_radiation_mj_m2", 
#  "rainfall_mm", 
#  "snowfall_cm", 
#  "season", 
#  "holiday", 
#  "functioning_day"
#  day <- weekdays(as.Date(bike_demand$date))
#  weekend

#  only use functioning days for analysis
bike_demand <- bike_demand[bike_demand$functioning_day == "Yes", ]
model_poisson_1 <- glm(
  rented_bike_count ~ rainfall_mm + season + hour, 
  family=poisson, data=bike_demand
)

summary(model_poisson_1)
Anova(model_poisson_1)

#  create categorical variable for hour
lateNightHours <- c(23,0,1)
earlyMorningHours <- 2:6
daytimeHours <- 7:15
nightHours <- 16:22
peakMorningHour <- 8
peakNightHour <- 18

bike_demand$hour_bin <- ""
bike_demand[bike_demand$hour %in% lateNightHours, "hour_bin"] <- "lateNight"
bike_demand[bike_demand$hour %in% earlyMorningHours, "hour_bin"] <- "earlyMorning"
bike_demand[bike_demand$hour %in% daytimeHours, "hour_bin"] <- "daytime"
bike_demand[bike_demand$hour %in% nightHours, "hour_bin"] <- "night"
bike_demand[bike_demand$hour == peakMorningHour, "hour_bin"] <- "peakMorning"
bike_demand[bike_demand$hour == peakNightHour, "hour_bin"] <- "peakNight"

model_poisson_2 <- glm(
  rented_bike_count ~ 
    rainfall_mm 
    + season 
    + temperature_c 
    + humidity 
    + wind_speed_m_s 
    + visibility_10m 
    + dew_point_temp_c 
    + solar_radiation_mj_m2 
    + rainfall_mm 
    + snowfall_cm
    + hour_bin
    + weekend, 
    family=poisson(link="log"), data=bike_demand
)

summary(model_poisson_2)

#  forward selection (extra...using BIC with k set to log(n))
model_poisson_empty <- glm(rented_bike_count ~ 1, family=poisson, data=bike_demand)
model_poisson_full <- model_poisson_2

model_poisson_fwd <- step(
  object=model_poisson_empty,
  scope=list(upper=model_poisson_full),
  direction="forward",
  k=log(nrow(bike_demand)),
  trace=TRUE
)
  
summary(model_poisson_fwd)
anova(model_poisson_fwd)

#  backwards elimination (extra...using BIC with k set to log(n))
model_poisson_empty <- glm(rented_bike_count ~ 1, family=poisson, data=bike_demand)
model_poisson_full <- model_poisson_2

model_poisson_bwd <- step(
  object=model_poisson_full,
  scope=list(upper=model_poisson_empty),
  direction="backward",
  k=log(nrow(bike_demand)),
  trace=TRUE
)

summary(model_poisson_bwd)
anova(model_poisson_bwd)
























