# Import the data set
library(readxl)
Case_1_ <- read_excel("Data .xlsx")
View(Case_1_)


#Question 1

#Computing the mean of every numeric variable in the data set.
mean(Case_1_$`Full-Time Enrollment`) 
mean(Case_1_$`Students per Faculty`)
mean(Case_1_$`Local Tuition ($)`)
mean(Case_1_$`Foreign Tuitiion ($)`)
mean(Case_1_$Age)
mean(Case_1_$`Starting Salary ($)`)

#converting Foreign variable into numeric value and computing mean
Foreign_Count<-(Case_1_$`Full-Time Enrollment`*Case_1_$`%Foreign`)/100   
mean(Foreign_Count)

#Computing the median of every numeric variable in the data set.
median(Case_1_$`Full-Time Enrollment`) 
median(Case_1_$`Students per Faculty`)
median(Case_1_$`Local Tuition ($)`)
median(Case_1_$`Foreign Tuitiion ($)`)
median(Case_1_$Age)
median(Case_1_$`Starting Salary ($)`)
median(Foreign_Count)

#Computing the Standard Deviation of every numeric variable in the data set.
sd(Case_1_$`Full-Time Enrollment`)
sd(Case_1_$`Students per Faculty`)
sd(Case_1_$`Local Tuition ($)`)
sd(Case_1_$`Foreign Tuitiion ($)`)
sd(Case_1_$Age)
sd(Case_1_$`Starting Salary ($)`)
sd(Foreign_Count)

#Computing the Variance of every numeric variable in the data set.
var(Case_1_$`Full-Time Enrollment`)
var(Case_1_$`Students per Faculty`)
var(Case_1_$`Local Tuition ($)`)
var(Case_1_$`Foreign Tuitiion ($)`)
var(Case_1_$Age)
var(Case_1_$`Starting Salary ($)`)
var(Foreign_Count)

#Computing the Range of every numeric variable in the data set.
range(Case_1_$`Full-Time Enrollment`)
range(Case_1_$`Students per Faculty`)
range(Case_1_$`Local Tuition ($)`)
range(Case_1_$`Foreign Tuitiion ($)`)
range(Case_1_$Age)
range(Case_1_$`Starting Salary ($)`)
range(Foreign_Count)

#Computing the Minimum of every numeric variable in the data set.
min(Case_1_$`Full-Time Enrollment`)
min(Case_1_$`Students per Faculty`)
min(Case_1_$`Local Tuition ($)`)
min(Case_1_$`Foreign Tuitiion ($)`)
min(Case_1_$Age)
min(Case_1_$`Starting Salary ($)`)
min(Foreign_Count)

#Computing the maximum of every numeric variable in the data set.
max(Case_1_$`Full-Time Enrollment`)
max(Case_1_$`Students per Faculty`)
max(Case_1_$`Local Tuition ($)`)
max(Case_1_$`Foreign Tuitiion ($)`)
max(Case_1_$Age)
max(Case_1_$`Starting Salary ($)`)
max(Foreign_Count)

#Question 2

#installing and loading required libraries 
install.packages("ggplot2")   
library(ggplot2) 
install.packages("dplyr")
library(dplyr) 

#Question 2a

#Plotting line graph for local tuition vs foreign tuition
ggplot(Case_1_,aes(Case_1_$`Local Tuition ($)`,Case_1_$`Foreign Tuitiion ($)`)) + geom_line(color="black") + geom_point(shape=21,color="black",fill="red",size=4)+ xlab("Local Tuition")+ ylab("Foreign Tuition")

#Question 2b

#Created new data set for required condition and computed mean
workexp_yes_sal <-filter(Case_1_,Case_1_$`Work Experience`=="Yes")
mean(workexp_yes_sal$`Starting Salary ($)`)

workexp_no_sal <-filter(Case_1_,Case_1_$`Work Experience`=="No")
mean(workexp_no_sal$`Starting Salary ($)`) 

#Question 2c

#Created new data set for required condition and computed mean
Eng_yes_sal <- filter(Case_1_,Case_1_$`English Test` == "Yes")
mean(Eng_yes_sal$`Starting Salary ($)`)

Eng_no_sal <- filter(Case_1_,Case_1_$`English Test` == "No")
mean(Eng_no_sal$`Starting Salary ($)`)

#Question 3

#Plotting line graph for local tuition vs starting salary
ggplot(Case_1_, aes(Case_1_$`Local Tuition ($)`,Case_1_$`Starting Salary ($)`)) + geom_line(color="black") + geom_point(shape=21,color="black",fill="red",size=4) +xlab("Local Tuition")+ylab("Starting Salary")
#Computed correlation coefficient for local tuition vs starting salary
cor(Case_1_$`Local Tuition ($)`,Case_1_$`Starting Salary ($)`,method = "pearson")

# Plotting graph for foreign tuition vs starting salary
ggplot(Case_1_ ,aes(Case_1_$`Foreign Tuitiion ($)`,Case_1_$`Starting Salary ($)`)) +geom_line(color="black")+geom_point(shape=21,color="black",fill="red",size=4)+ xlab("Foreign Tuition")+ ylab("Starting Salary")
#Computed correlation coefficient for foreign tuition vs starting salary
cor(Case_1_$`Foreign Tuitiion ($)`,Case_1_$`Starting Salary ($)`,method = "pearson")


#Question 4

# Graphical representation of Starting salary using BoxPlot 
boxplot(Case_1_$`Starting Salary ($)`, horizontal=TRUE, xlab="Starting Salary", col=I("Pink"), main="Starting Salary")

#Computing Quartiles 
quantile(Case_1_$`Starting Salary ($)`,p = 0.25) #Q1
quantile(Case_1_$`Starting Salary ($)`,p = 0.50) #Q2
quantile(Case_1_$`Starting Salary ($)`,p = 0.75) #Q3


#Computing minimum and maximum values of boxplot
quantile(Case_1_$`Starting Salary ($)`,p = 0.25) - 1.5 * IQR(Case_1_$`Starting Salary ($)`)
quantile(Case_1_$`Starting Salary ($)`,p = 0.75) + 1.5 * IQR(Case_1_$`Starting Salary ($)`)

