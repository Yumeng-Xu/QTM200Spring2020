---
#Project: QTM200_PS3
#Name: Yumeng Xu
#Date: 2/12/2020"

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/QTM200Spring2020/problem_sets/PS3")

#####################
# Problem 1
#####################

#We are interested in knowing how the difference in campaign spending between incumbent and challenger affects the incumbent's vote share.
#1. Run a regression where the outcome variable is voteshare and the explanatory variable is difflog.

#Import data
incum <- read.csv("incumbents_subset.csv")
#run the regression with y=voteshare, x=difflog
regression_p1 <- lm(voteshare ~ difflog, data=incum)
#check the summary of model with coefficient estimates 
summary(regression_p1)


#2. Make a scatterplot of the two variables and add the regression line.
pdf("plot1.pdf")
plot(incum$difflog, incum$voteshare,
xlab = "Vote Share", ylab = "difference in spending")
abline(lm(incum$voteshare ~ incum$difflog)) #add the regression line
dev.off()

#3. Save the residuals of the model in a separate object.
residuals_1 <- regression_p1$residuals

#4. Write the prediction equation.
#The prediction equation is taken the form of Y^i = β0 + β1*Xi.
regression_p1$coefficients #check the values of beta0 and beta1.
# Y^i = 0.579 + 0.042*Xi


#####################
# Problem 2
#####################

#We are interested in knowing how the dfference between incumbent and challenger's spending and the vote share of the presidential candidate of the incumbent's party are related.
#1. Run a regression where the outcome variable is presvote and the explanatory variable is difflog.

#run the regression with y=presvote, x=difflog
regression_p2 <- lm(presvote ~ difflog, data=incum)
#check the summary of model with coefficient estimates 
summary(regression_p2)

#2. Make a scatterplot of the two variables and add the regression line.
pdf("plot2.pdf")
plot(incum$difflog, incum$presvote,
xlab = "presidential vote", ylab = "difference in spending")
abline(lm(incum$presvote ~ incum$difflog)) #add the regression line
dev.off()

#3. Save the residuals of the model in a separate object.
residuals_2 <- regression_p2$residuals

#4. Write the prediction equation.
regression_p2$coefficients #check the values of beta0 and beta1.
# Y^i = 0.507 + 0.024*Xi


#####################
# Problem 3
#####################

#We are interested in knowing how the vote share of the presidential candidate of the incumbent's party is associated with the incumbent's electoral success.
#1. Run a regression where the outcome variable is voteshare and the explanatory variable is presvote.

#run the regression with y=voteshare, x=presvote
regression_p3 <- lm(voteshare ~ presvote, data=incum)
#check the summary of model with coefficient estimates 
summary(regression_p3)

#2. Make a scatterplot of the two variables and add the regression line.
pdf("plot3.pdf")
plot(incum$presvote, incum$voteshare,
xlab = "presidential vote", ylab = "vote share")
abline(lm(incum$voteshare ~ incum$presvote)) #add the regression line
dev.off()

#3. Write the prediction equation.
regression_p3$coefficients #check the values of beta0 and beta1.
# Y^i = 0.441 + 0.388*Xi


#####################
# Problem 4
#####################

#The residuals from part (a) tell us how much of the variation in voteshare is not explained by the difference in spending between incumbent and challenger. The residuals in part (b) tell us how much of the variation in presvote is not explained by the difference in spending between incumbent and challenger in the district.
#1. Run a regression where the outcome variable is the residuals from Question 1 and the explanatory variable is the residuals from Question 2.

#run the regression with y=residuals_1, x=residuals_2
regression_p4 <- lm(residuals_1 ~ residuals_2)
#check the summary of model with coefficient estimates 
summary(regression_p4)

#2. Make a scatterplot of the two variables and add the regression line.
pdf("plot4.pdf")
plot(residuals_2, residuals_1,
xlab = "Residuals in Voteshare", ylab = "Residuals in Presidential Vote")
abline(lm(residuals_1 ~ residuals_2)) #add the regression line
dev.off()

#3. check the values of beta0 and beta1.
regression_p4$coefficients
# Y^i = -4.860 + 0.256*Xi


#####################
# Problem 5
#####################

#What if the incumbent's vote share is affected by both the president's popularity and the difference in spending between incumbent and challenger?
#1. Run a regression where the outcome variable is the incumbent's voteshare and the explanatory variables are difflog and presvote.

#run the regression with y=voteshare, x=difflog and presvote
regression_p5 <- lm(voteshare ~ difflog+presvote, data=incum)
#check the summary of model with coefficient estimates 
summary(regression_p5)

#2. Write the prediction equation.
regression_p5$coefficients #check the values of beta0 and beta1.
# Y^i = 0.4486 + 0.0355*Xi(difflog) + 0.2569*Xi(presvote)