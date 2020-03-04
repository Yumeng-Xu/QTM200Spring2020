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
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("faraway"),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS5")


#####################
# Problem 5
#####################

# load data
install.packages("teengamb")
gamble <- (data=teengamb)
install.packages("car")
library("car") 
# run regression on gamble with specified predictors
model1 <- lm(gamble ~ sex + status + income + verbal, gamble)

#Check constant variance
pdf("a.pdf")
res <- model1$residuals
fitted <- model1$fitted.values
plot(res ~ fitted, data = gamble)
abline(h=0)
dev.off()

#Check the normality
qqplot(model1)

#Check for high leverage points
pdf("c.pdf")
hatvalues(model1)
str(model1) #n=47
plot(hatvalues(model1))
abline(h=2*3/47, lty=2)
abline(h=3*3/47, lty=3)
dev.off()

#Check for outliers.
outlierTest(model1)

#Check for influential points
influencePlot(model1, sub="Circlee size is proportial to Cook's Distance")
