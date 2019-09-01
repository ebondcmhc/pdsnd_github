# Read in data and load libraries
ny = read.csv('new-york-city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

library(ggplot2)
library(dplyr)

#Combine data into a new frame and create a new column identifying each city
combined = bind_rows(list(ny = ny, chi = chi, wash = wash), .id="City")

#Inspect combined data.  Washington has NA for Gender and Birth.Year as expected.
head(combined)
tail(combined)

#Question 1: How do trip durations compare between cities?

#Explore distribution of trip duration by city
#Trip duration is in seconds, so /60 for minutes
qplot(x = City, y = Trip.Duration/60,
      data = combined,
      xlab = "City",
      ylab = "Trip Duration (minutes)",
      main = "Distribution of trip duration by city",
      geom = 'boxplot')

#We see that there are some outlier trips with unusually long lengths, while most trips are much shorter

#Get summary statistics of trip duration by city
by(combined$Trip.Duration/60, combined$City, summary)

#Most trips are under 30 min, so re-plot with y-axis limit and tidy up plot
qplot(x = City, y = Trip.Duration/60,
      data = combined,
      xlab = "City",
      ylab = "Trip Duration (minutes)",
      main = "Trip durations were generally shortest in New York and longest in Washington",
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,30))

#

#Explore counts of users by type for each city
by(combined$City, combined$User.Type, summary)

#There is only one trip with type "Dependent" and a few values are blank for NY

#Consider only user types "Customer" and "Subscriber" and plot
ggplot(data = subset(combined, User.Type == "Customer" | User.Type == "Subscriber"),
       aes(x = City, fill = User.Type)) +
  ggtitle("Subscribers take many more trips than occasional customers across all cities") +
  labs(x = "City", y = "Number of trips", fill = "User Type") +
  geom_bar(stat="count", position=position_dodge())

#Get summary statistics for trip duration by type of user (all cities combined)
by(combined$Trip.Duration/60, combined$User.Type, summary)

#Trips for subscribers are quite a bit shorter than those for customers.  Plot trips up to 45 min based on summary statistics.

#Explore distribution of trip duration by user type (all cities combined)
#Restrict plot to user types "Customer" and "Subscriber"
qplot(x = User.Type, y = Trip.Duration/60,
      data = subset(combined, User.Type == "Customer" | User.Type == "Subscriber"),
      xlab = "Type of user",
      ylab = "Trip Duration (minutes)",
      main = "Occasional customers take longer trips than subscribers (all cities combined)",
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0,40))






