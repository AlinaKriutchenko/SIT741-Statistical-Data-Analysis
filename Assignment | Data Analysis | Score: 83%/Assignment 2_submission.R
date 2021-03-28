#Add Swan hospital dataset:
install.packages("readr")
install.packages("magrittr") 
install.packages("dplyr")    
library(magrittr) 
library(dplyr) 
install.packages("tidyverse")
library(tidyverse)

#=========================================================
govhack3 <- 'govhack3.csv'
       
top_row <- read_csv(govhack3, col_names = FALSE, n_max = 1)
second_row <- read_csv(govhack3, n_max = 1)

column_names <- second_row %>% 
  unlist(., use.names=FALSE) %>% 
  make.unique(., sep = "__") # double underscore

column_names[2:8] <- str_c(column_names[2:8], '0', sep='__')

daily_attendance <- 
  read_csv(govhack3, skip = 2, col_names = column_names)
#=========================================================

#Add days of week column
daily_attendance$Week_days <- rep_len(1:7, length.out=365)

#=========================================================

#Gather:
swan <- gather(daily_attendance, "Value", "Cases", 2:8)
#Choosing rows by hospital:
swan$Index <- rep_len(7, length.out=2555)
#Select columns for hospital
swan <- select(swan, Date, Week_days, Value, Cases, Index)
#Subset index:
swan <- subset(swan, select = -c(Index) )
#Delete last three characters in the Value column:
swan$Value = substr(swan$Value,1,nchar(swan$Value)-3)

#=========================================================

#Are the variables having the expected variable types in R? 
summary(swan)
#Clean up the data types:
swan$Cases <- as.numeric(as.character(swan$Cases))
summary(swan)
#replace missing values
swan[c("Cases")][is.na(swan[c("Cases")])] <- 0
swan$Days <- rep_len(1:365, length.out=2555)

#=========================================================

#Subset attendances:
attendances <- swan[swan[, "Value"] == 'Attendance',]
#Subset admissions:
admissions <- swan[swan[, "Value"] == 'Admissions',]

#=========================================================
#==============PREPARING DATA FOR THE TASK 4==============
#=========================================================

#READ THE WEATHER DATA:
weather <- read.csv("2286078.csv")
#Check which stations have all 365 observations:
data.frame(table(weather$NAME))

#Select observations from the "PERTH AIRPORT, AS" weather station.
perth_airport <- weather[8122:8486,]
#Remove empty columns:
perth_airport <- perth_airport[ -c(1,7:20) ]
#Select DATE, daily temperature and precipitation data:
perth_airport <- perth_airport[ c(1,5:13) ]
#Remove "NAME" column:
perth_airport <- perth_airport[ -c(1,1) ]

#================================
#Import:
library(data.table)
library(RcppRoll)
install.packages("roll")
#Create a column of moving average mean temperature:
perth_airport$MOVING <- frollmean(perth_airport$TAVG, 3)
perth_airport$MOVING
#Import additional data from June 2013 to count moving average:
weather_2 <- read.csv("2290898_2.csv")
#Add the missing values:
perth_airport[1, 10] = 54
perth_airport[2, 10] = 55

#================================
#Install packages
install.packages("smooth")
require(smooth)
install.packages("Mcomp")
require(Mcomp)
#Plot moving average mean temperature
plot <- sma(perth_airport$TAVG, h=3, silent=FALSE)

#Select MOVING AND PRCP columns from the perth_airport:
attendances$Moving <- perth_airport$MOVING
attendances$Prcp <- perth_airport$PRCP

#=========================================================
#=========================================================
#===========================LM 1==========================  Task 3.2
#=========================================================
#=========================================================
#Fit the linear model with Days:
linear.fit1 = lm(Cases~Days, data=attendances)
sum1 <- summary(linear.fit1)
sum1


#Install fBasics package
library(fBasics)
jarqueberaTest(linear.fit1$resid) #Test residuals for normality

cor(attendances$Cases, attendances$Days)
#=========================================================
#===========================LM 2==========================  Task 3.4
#=========================================================
#Fit the linear model with Days and Week_days:
linear.fit2 = lm(Cases~Days+Week_days, data=attendances)
sum2 <- summary(linear.fit2)
sum2
jarqueberaTest(linear.fit2$resid)
#=========================================================
#===========================LM 3==========================  Task 4.2
#=========================================================
#Fit the linear model with Days, Week_days and Moving:
linear.fit3 = lm(Cases~Days+Week_days+Moving, data=attendances)
sum3 <- summary(linear.fit3)
sum3
jarqueberaTest(linear.fit3$resid) 
#=========================================================
#===========================LM 4==========================  Task 4.3
#=========================================================
#Fit the linear model with Days, Week_days, Moving and Prcp
linear.fit4 = lm(Cases~Days+Week_days+Moving+Prcp, data=attendances)
sum4 <- summary(linear.fit4)
sum4
jarqueberaTest(linear.fit4$resid)

#=========================================================
#=========================================================
#=====================PLOT for LM 1=======================  Task 3.2
#=========================================================
#=========================================================
#Days(DATE) & Residuals Plot
plot(linear.fit1$resid~attendances$Days[order(attendances$Days)],
     main="Date x Residuals\nfor Simple Regression",
     xlab="Number of attendances", ylab="Residuals", col = "medium sea green", pch = 18)
abline(h=0,lty=2, col = "blue violet")
#================================
#Histogram of Residuals
hist(linear.fit1$resid, main="Histogram of Residuals", col = "papaya whip",
     ylab="Residuals")
#wheat
#================================
#Q-Q Plot
qqnorm(linear.fit1$resid,col = "tan")
qqline(linear.fit1$resid,col = "purple")

#=========================================================
#=====================PLOT for LM 2=======================  Task 3.4
#=========================================================
#Week & Residuals Plot2
plot(linear.fit2$fitted, rstudent(linear.fit2),
     main="Multi Fit Studentized Residuals",
     xlab="Predictions 2",ylab="Studentized Resid 2", col = "light steel blue", pch = 15,     #medium aqua marine
     ylim=c(-2.5,2.5))
abline(h=0, lty=2, col = "royal blue")
#================================
#Histogram2 of Residuals
hist(linear.fit2$resid,main="Histogram of Residuals", col = "lavender")   #wheat
#================================
#Q-Q plot2
qqnorm(linear.fit2$resid, col = "violet")
qqline(linear.fit2$resid, col = "dark magenta")

#=========================================================
#=====================PLOT for LM 3=======================Task 4.2  
#=========================================================
#Week & Residuals Plot3
plot(linear.fit3$fitted, rstudent(linear.fit3),
     main="Multi Fit Studentized Residuals",
     xlab="Predictions 3",ylab="Studentized Resid 3", col = "orange", pch = 15,     #medium aqua marine
     ylim=c(-2.5,2.5))
abline(h=0, lty=2, col = "royal blue")
#================================
#Histogram3 of Residuals
hist(linear.fit3$resid,main="Histogram of Residuals", col = "sandy brown")   #wheat
#================================
#Q-Q plot3
qqnorm(linear.fit3$resid, col = "orange")
qqline(linear.fit3$resid, col = "dark magenta")

#=========================================================
#=====================PLOT for LM 4=======================  Task 4.3
#=========================================================
#Week & Residuals Plot4
plot(linear.fit4$fitted, rstudent(linear.fit4),
     main="Multi Fit Studentized Residuals",
     xlab="Predictions 4",ylab="Studentized Resid 4", col = "orchid", pch = 15,     #medium aqua marine
     ylim=c(-2.5,2.5))
abline(h=0, lty=2, col = "royal blue")
#================================
#Histogram2 of Residuals
hist(linear.fit4$resid,main="Histogram of Residuals", col = "plum")   #wheat
#================================
#Q-Q plot2
qqnorm(linear.fit4$resid, col = "light pink")
qqline(linear.fit4$resid, col = "slate gray")

#=========================================================
#=========================================================
#==========================GGPLOT 1=======================  Task 3.2
#=========================================================
#=========================================================

#Installing 'ggpubr' by R
library(ggpubr)
theme_set(theme_pubr())


ggplot(attendances, aes(x = Days, y = Cases)) +
  geom_point() +
  stat_smooth()
#================================

cor(attendances$Cases, attendances$Days)

#================================
#Add the regression line
ggplot(attendances, aes(Days, Cases)) +
  geom_point() +
  stat_smooth(method = lm)

#=========================================================
#==========================GGPLOT 2=======================  Task 3.4
#=========================================================
ggplot(attendances, aes(x = Days+Week_days, y = Cases)) +
  geom_point() +
  stat_smooth()

#================================
#Add the regression line
ggplot(attendances, aes(Days+Week_days, Cases)) +
  geom_point() +
  stat_smooth(method = lm)

#=========================================================
#==========================GGPLOT 3=======================  Task 4.2
#=========================================================
ggplot(attendances, aes(x = Days+Week_days+Moving, y = Cases)) +
  geom_point() +
  stat_smooth()
#================================
#Add the regression line
ggplot(attendances, aes(Days+Week_days+Moving, Cases)) +
  geom_point() +
  stat_smooth(method = lm)

#=========================================================
#==========================GGPLOT 4=======================  Task 4.3
#=========================================================
ggplot(attendances, aes(x = Days+Week_days+Moving+Prcp, y = Cases)) +
  geom_point() +
  stat_smooth()

#================================
#Add the regression line
ggplot(attendances, aes(Days+Week_days+Moving+Prcp, Cases)) +
  geom_point() +
  stat_smooth(method = lm)

#=========================================================
#=========================================================
#===========================GAM 1=========================  Task 3.3
#=========================================================
#=========================================================
#Load the package MGCV:
library(mgcv)
gam1 <- gam(Cases ~ s(Days, k=3),
            family=Gamma(link=log),
            data=attendances)
gam1
summary(gam1)
plot(gam1)
plot(gam1, residuals = TRUE)
gam.check(gam1)
#=========================================================
#===========================GAM 2=========================  Task 3.4
#=========================================================
gam2 <- gam(Cases ~ s(Days) + s(Week_days, k=3),
            family=Gamma(link=log),
            data=attendances)
gam2
summary(gam2)
plot(gam2)
plot(gam2, residuals = TRUE)

gam.check(gam2)
#=========================================================
#===========================GAM 3=========================  Task 4.2
#=========================================================
gam3 <- gam(Cases ~ s(Days) + s(Moving) + s(Week_days, k=3),
            family=Gamma(link=log),
            data=attendances)
gam3
summary(gam3)
plot(gam3)
plot(gam3, residuals = TRUE)
gam.check(gam3)
#=========================================================
#===========================GAM 4=========================  Task 4.3
#=========================================================
gam4 <- gam(Cases ~ s(Days) + s(Week_days, k=4) + s(Moving) + s(Prcp),
            family=Gamma(link=log),
            data=attendances)
gam4
summary(gam4)
plot(gam4)
plot(gam4, residuals = TRUE)
gam.check(gam4)


#=========================================================
#============================AIC==========================
#=========================================================

AIC(linear.fit1)
AIC(gam1)
AIC(linear.fit2)
AIC(gam2)

AIC(linear.fit3)
AIC(gam3)
AIC(linear.fit4)
AIC(gam4)