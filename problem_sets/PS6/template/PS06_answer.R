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

lapply(c(),  pkgTest)

install.packages("nnet")
library(nnet)
install.packages("MASS")
library(MASS)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS6")

#####################
# Problem 1
#####################
#Import Data
cholesterol <- read.csv("cholesterol.csv")

#1)
model_additive <- lm(cholCat ~ sex + fat, data=cholesterol)
summary(model_additive)

#H0: Neither sex nor fat has an effect on the cholesterol.
#H1: At least one variable has a relationship with the cholesterol.
#Since the p-values for both variables (sex and fat) are smaller than 0.05, 
#We have enough evidence to reject the null and conclude that the two variables have relationship with the cholesterol.


#2)
cholesterol = -0.1303597 + 0.1894* sex + 0.0082466* fat
#a)
#For females, sex = 0, the probability of having high cholesterol increases by 0.8% if their fat increases by 1g per day.

#b)
#For males, sex = 1, the probability of having high cholesterol increases by 18.8% if their fat increases by 1g per day.

#c)
#sex = 0, fat = 100,
cholesterol = -0.1303597 + 0.1894* 0 + 0.0082466* 100 = 0.6943003
#The estimated probability of a woman with a fat intake of 100 grams per day being in the high cholesterol group is 0.694.

#d)
model_int <- lm(cholCat ~ sex + fat+ sex*fat, data=cholesterol)
summary(model_int)
#No, the answer would not change.
#After we add in the interaction term, we do not see a significant increase in the explanatory power.


#####################
# Problem 2
#####################

#Import data
gdp <- read.csv("gdpChange.csv")

#1)
gdp_output <- relevel(gdp$GDPWdiff, ref="no change")
model_unorder <- multinom(out ~ REG + OIL, data=gdp, Hess = T)
summary(model_unorder)
#for every one unit increase in X, the log-odds of Yj moving from Y1 increase by B 
##2)
gdp_out <- ordered(gdp_output,levels=c("negative","no change","positive"))
polr(gdp_out ~ REG + OIL, data=gdp, Hess=T)
