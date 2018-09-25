#upload data
Week1 <- read.csv("Week1.csv", header = T)
#check structure of data
str(Week1)
#maximum value of the variable "ID"
max(Week1$ID)
#minimum value of the variable "Beat"
min(Week1$Beat)

#observations have value TRUE in the Arrest variable
table(Week1$Arrest)

# observations have a LocationDescription value of ALLEY?
table(Week1$LocationDescription)

#convert date column into right format
Week1$Date <-  as.Date(strptime(Week1$Date, "%m/%d/%y %H:%M"))

#Extract month, weekdays
Week1$Month = months(Week1$Date)
Week1$Weekday = weekdays(Week1$Date)

#convert month variable into a factor
Week1$Month <- as.factor(Week1$Month)

#median date in the dataset
summary(Week1$Month)

#weekday with most vehicle theft
Week1$Weekday <- as.factor(Week1$Weekday)
summary(Week1$Weekday)


# plot histogram with variable date
hist(Week1$Date,breaks=100)

# boxplot of the variable "Date", sorted by the variable "Arrest"
boxplot(Week1$Date ~ Week1$Arrest)

table(Week1$Year, Week1$Arrest)

# sort data to see the top 5 locations with theft
sort(table(Week1$LocationDescription))

#subset data with the top 5 locations with theft
top5 <- subset(Week1, LocationDescription == "STREET" | LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)"|
               LocationDescription == "ALLEY" | LocationDescription == "DRIVEWAY - RESIDENTIAL" | 
               LocationDescription == "GAS STATION")

#convert location description into a factor
top5$LocationDescription = factor(top5$LocationDescription)
str(top5)

# group top 5 location by number of arrest
tapply(top5$Arrest, top5$LocationDescription, summary, na.rm = TRUE)

#top 5 location by number of arrest per weekday
table(top5$LocationDescription, top5$Weekday)

