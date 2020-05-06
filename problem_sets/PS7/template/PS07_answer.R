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

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS7")

install.packages("lme4")
library("Matrix")
library("lme4")
library("ggplot2")
install.packages("googleVis")
library("googleVis")
install.packages("sjPlot")
library("sjPlot")

lapply(c("sjPlot", "googleVis"),  pkgTest)

#####################
# Problem 1
#####################

#Import data
mexico_elections <- read.csv("MexicoMuniData.csv", stringsAsFactors = F, header=T)
#a) 
model <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, family = "poisson", data=mexico_elections)
model
anova(model, test = "Chisq")
#Since the p-value for competitive district is greater than 0.05, there is no enough evidence to support that
#whether the district is highly contested or not has a statistically significant effect on the number of times PAN presideential candidates visited.

#b)
#The coefficient for marginality.06 is -2.098 means that keeping other variables constant, the expected log count for a one-unit increase in marginality.06 is -2.098.
#In other words, the district being a safe seat would decrease the log odds of marginality.06 by 2.098.
#The coefficient for PAN.governor.06 is -0.207 means that keeping other variables constant, the expected log count for a one-unit increase in PAN.governor.06 is -0.207

#c)
Y = -3.9304 - 0.4594*competitive.district - 2.0981* marginality.06 - 0.2073*PAN.governor.06  
#When competitive.district = 1, marginality.06 = 0, and PAN.governor.06 = 1,
#Y would be -4.60.

#####################
# Problem 2
#####################

#1)
pooled_1 <- lm(Reaction ~ Days, data=lme4)
#2)
unpooled_2 <- lm(Subject, data=lme4)
#3)
unpooled_3 <- lm(Reaction ~ Subject:Days, data=lme4)
#4)
unpooled_4 <- lm(Reaction ~ Subject + Days + Subject:Days, data=lme4)
#5)不会写

