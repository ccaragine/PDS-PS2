########################### PDS HW 2 #######################################
## Author: Crystal Caragine
## Date: 2/5/2020
## Class: Political Data Science
## Saved As: PDS_HW2_Caragine

################### Question 1 
for (i in 1:7){
  print(i^3) }

################## Question 2 
set.seed(14)
dice <- sample(2:12, size=1000, replace=TRUE) 

x <- sample(dice,1)
print(x)
while(x!=2 & x!=6 & x<8){
  x <- sample(dice, 1)
  print(x) 
}

          
##################### Question 3 
GSS <- read.csv("C:/Users/cmcar/OneDrive/Documents/School/Stats/Political Data Science/GSS-data.csv")

vote.choice <- function(i){
  if(i=="trump")
    print(summary(GSS$pres16)[8])
  if(i=="clinton")
    print(summary(GSS$pres16)[2])
  if(i=="other")
    print(summary(GSS$pres16)[7])
  if(i != "trump" & i!="clinton" & i!="other")
    warning('Please enter either Trump Clinton or Other 
             into the func to return a valid response')
}
  
vote.choice("trump")
vote.choice("clinton")
vote.choice("other")
vote.choice("mydog")
    
#################### Question 4 
#install.packages("fivethirtyeight") 
library(fivethirtyeight) 
library(plyr)

summary(cabinet_turnover) 
CT <- cabinet_turnover

president <- c("Carter", "Reagan", "Bush 41", "Clinton", "Bush 43", "Obama", "Trump")
served <- c(1461, 2922, 1461, 2922, 2922, 2922, 1105) 
pres_serv <- cbind(president, served)
PRES_SERV <- as.data.frame(pres_serv)

MERG <- merge(PRES_SERV, CT, "president")
MERG2 <- na.omit(MERG) 


avglength <- ddply(MERG2, .(president), summarize, length=mean(length))

percent <- avglength[2]/served
PER <- cbind(president, percent)

appoint <- function(i){
  if(i=="Carter")
    print(PER$length[1])
  if(i=="Reagan")
    print(PER$length[2])
  if(i=="Bush 41")
    print(PER$length[3])
  if(i=="Clinton")
    print(PER$length[4])
  if(i=="Bush 43")
    print(PER$length[5])
  if(i=="Obama")
    print(PER$length[6])
  if(i=="Trump")
    print(PER$length[7])
}
  
appoint("Trump")

################### Question 5
library(dplyr)
summary(congress_age)
CA <- congress_age

aggregate(CA[, "age"], list(CA$congress), mean)
aggregate(CA[, "age"], list(CA$state), mean)


congress_stats <- function(stat){
 print( aggregate(CA[, "age"], list(stat), mean) )}

congress_stats(CA$congress)
congress_stats(CA$state)




    