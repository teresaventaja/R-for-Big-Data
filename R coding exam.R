#Question_1
#1.a
hurricanes <- read.csv(file="hurricanes.csv", head=TRUE, sep=",")
#1.b
#Getting only the collumn about pressure
x <- c(hurricanes$LF.PressureMB)
#Calculating the average and rounding to 2 decimals
round(mean(x),digits=2)
#1.c
hurricanes$X[which.max(as.numeric(hurricanes$deaths))]
#1.d
multiple <- subset(hurricanes, LF.times != "1")
#1.e
#Selecting the damage ratio only
DamageRatio<-(multiple$BaseDamage/multiple$BaseDam2014)
#Selecting the names only
MultipleNames<- (multiple$Name)
#Creating a data frame for them
data.frame(MultipleNames, DamageRatio)
#1.f
#Filtering by the years included
a <- subset(hurricanes, Year >= '1990' & Year <= '2000')
#Calculating the lowest
hurricanes$X[which.min(as.numeric(a$NDAM2014))]
#1.g
#Filtering for male
b <- subset(hurricanes, mf == 'm')
deathsmale <- c(b$deaths)
#average deaths for male
d <- mean(deathsmale)
#Filtering for female
c <- subset(hurricanes, mf == 'f')
deathsfemale <- c(c$deaths)
#average deaths for female
e <- mean(deathsfemale)
#Asking the console if the average of deaths for male is lower than for female
d < e
#1.h
#Subsetting
maxwindspeed <- subset(hurricanes$LF.WindsMPH, hurricanes$LF.WindsMPH >= '120')
#Creating the file CSV
write.csv(maxwindspeed, 'high_winds.csv', row.names = FALSE)
#Question_2
#2.a
plot(hurricanes$LF.WindsMPH, hurricanes$LF.PressureMB, pch=21, bg=c("red","green3","blue")
       [hurricanes$mf], main="Edgar Anderson's Iris Data")
#2.b
plot(multiple$deaths, multiple$BaseDamage, pch=21, bg=c("red","green3","blue")
     [multiple$mf], main="Edgar Anderson's Iris Data")
#2.c
#Installing collour pallete
install.packages('viridis')
library(viridisLite)
cols=viridis(13)
#Creating histogram
hist(hurricanes$Year, 
          border="red",
          col=viridis(13), 
          main="Histogram of the years", 
          xlab = "Years",
          las=1, 
          breaks=13)
#Question_3
#3.a
var_true <- as.numeric('TRUE')
#3.b
j <- as.numeric(5)
if (class(TRUE) == class(j)){
  print('Success!')
} else {
  print('Failure')
}
#Question_4
#4.a
height = as.numeric(readline(prompt = "Enter your height(cm): "))
#4.b
height = as.numeric(readline(prompt = "Enter your height(cm): "))
#you need to enter the number on the console before running the following statement
print(paste(height))
#4.c
inches <- 0.393701
height_inches <- height * inches
#run the following code to get the calculation
height_inches
#4.d
if (height_inches>78){
  print('Too tall!')
} else if (height_inches>55) {
  print('Normal')
} else {
  print('Too short')
}

     

