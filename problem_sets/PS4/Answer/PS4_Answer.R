#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
install.packages("car")
library(car)
data(Prestige)
help(Prestige)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS4/Answer")


#####################
# Problem 1
#####################
#(a) Create a new variable professional by recoding the variable type so that professionals
#are coded as 1, and blue and white collar workers are coded as 0 (Hint: ifelse.)
#Check the levels of the variable
levels(Prestige$type)
#Store the edited varaible into a newe variable called "Prof"
Prof <- ifelse(Prestige$type=="prof",1,0)
str(Prof)

#(b) Run a linear model with prestige as an outcome and income, professional, and the
#interaction of the two as predictors (Note: this is a continuous x dummy interaction.)
lm(prestige~income+Prof+income:Prof, data=Prestige)

#(c) Write the prediction equation based on the result.
#Yi= 21.14 + 0.003*X1(income) + 37.78*X2(Prof) - 0.002*D(income:Prof)

#(d) Interpret the coefficient for income.
#The coefficient is a positive number, indicating that there is a positive relationship between income and prestige. 
# The coefficient of income is 0.003, which means, controlling other variables, a unit increase in average income will result in 0.3% increase in the score of prestige.

#(e) Interpret the coefficient for professional.
#The coefficient is a positive number, indicating that there is a positive relationship between professional and prestige. 
#The coefficient of professional is 37.78, which means, controlling income, a unit increase in type of occupation (i.e, from blue and white collar to professionals) will result in 3778% increase in the score of prestige.

#(f) Recall the prediction equation: Yi= 21.14 + 0.003*X1(income) + 37.78*X2(Prof) - 0.002*D(income:Prof)
#Since we are interestd in the effect of income on profeessional occupations, D(income:Prof) should equal to 1.
#So we get Yi= 21.14 + 0.003*X1(income) + 37.78*1 - 0.002*1*income = 58.92 + 0.001*income, where 0.001*income is the marginal effect.
#When income equals to 1000, the marginal effect would be 0.001*1000 = 1.
#Therefore, the effect of a 1000 increase in incomee will lead to 1 prestige score increase for professional occupations.

#(g) What is the effect of changing one's occupations from non-professional to professional
#when her income is $6,000? We are interested in the marginal effect of professional
#jobs when the variable income takes the value of 6, 000. Calculate the change in ^y based on your answer for (c).

#Recall the prediction equation: Yi= 21.14 + 0.003*X1(income) + 37.78*X2(Prof) - 0.002*D(income:Prof)
#When income equals to 6000, the prediction equation would be Yi= 21.14 + 0.003*6000 + 37.78*X2(Prof) - 0.002*6000*Prof = 39.14 + 25.78*Prof
#When one's occupation is non-professional, prof = 0, Yi =39.14. 
#When onee's occupation is professional, prof = 1, Yi = 64.92.
#The difference between the two equations is 25.78, which means th effect of changing one's occupations from non-professional to professional, given her income is 6000, is 25.78 unit increase in the prestige score.

#A

#Hypothesis: Ho:Beta2 = 0 vs. Ha:Beta2 is not 0.
#Test statistic: 
t=(0.042-0)/0.016=2.625
#p-value:
#df= n-3 parameters = 131-3 = 128
2*pt(2.625, 128, lower.tail = F)
#p-value = 0.0097
#p-value is 0.0097, which is smaller than the confidence lvel 0.05.
#We have enough evidence to reject the null hypothesis that there is no relationship between the lawn signs and the vote share.
#In other words, we can say that having these yard signs does affect the vote share.

#B
#Hypothesis: Ho:Beta3 = 0 vs. Ha:Beta3 is not 0.
#Test statistic: 
t=(0.042-0)/0.013=3.230
#p-value:
#df= n-3 = 131-3 = 128
2*pt(3.23, 128, lower.tail = F)
#p-value = 0.0016
#p-value is 0.0016, which is smaller than the confidence lvel 0.05.
#We have enough evidence to reject the null hypothesis that there is no relationship between living next to lawn signs and the vote share.
#In other words, we can say that adjacent to these yard signs does affect the vote share.

#C
#The coefficient of the constant equals to 0.302, which means when there is no lawn sign either in or near the precinct,
#the average proportion of the vote that went to Ken Cuccinelli is 30.2%.

#D
#The model does not work good enough to find out the influential factors that affect the vote share.
#As we can learn from the value of R square, which is 0.094 in this case, only 9.4% of variation is explained by this model.
#90.6% of variation cannot be explained by having lawn signs or not. So there should be other explanotary variables that have significant impact on the vote hare but is not included in this model.
#And these omitted variables might have greater explanotary power than yard signs.