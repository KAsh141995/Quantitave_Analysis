#Import Data set
library(readxl)
Data <- read_excel("Data.xlsx")
View(Data)

#Question 2

#Computing the mean for current and new golf ball sample data 
mean(Data$Current)
mean(Data$New)


#Computing the standard deviation for current and new golf ball sample data 
sd(Data$Current)
sd(Data$New)

#Computing the test statistic, df and p value for current and new golf ball sample data
data_sample<-t.test(x=Data$Current,y=Data$New)
data_sample$parameter
data_sample$statistic
data_sample$p.value


#Question 3

#computing the interval estimate with 95% confidence level 

#current golf ball sample
current_sample<-t.test(Data$Current)
current_sample$conf.int

#New golf ball sample
new_sample<-t.test(Data$New)
new_sample$conf.int

#Difference of sample mean for current and new golf ball 
data_sample$conf.int

#Question 4

#Box plot to check for outliers in the data

#For Current golf ball sample 
boxplot(Data$Current,col=I("pink"),horizontal = T,xlab="Driving distance ", main= "Current Golf Ball sample")

#For New golf ball sample
boxplot(Data$New,col=I("lightblue"),horizontal = T,xlab="Driving distance", main= "New Golf Ball sample")










