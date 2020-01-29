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

library(ggplot2)
lapply(c(),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2020/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#Since n<30, I use t-score
t90 <- qt ((1-0.90)/2, df= (25-1), lower.tail = FALSE)
sample_mean <- mean (y)
sample_sd <- sd (y)
lower_90 <- sample_mean-(t90 * (sample_sd/sqrt(25)))
upper_90 <- sample_mean+(t90 * (sample_sd/sqrt(25)))
CI90 <- c(lower_90, upper_90)
CI90 #The confidence interval is (93.96, 102.92).

#####################
# Problem 2
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)


#####################
# Problem 3
#####################
#Import the data
expenditure <- read.table("expenditure.txt", header=T)

#Plot the relationships among Y, X1, X2, and X3.
plot(expenditure$X1, expenditure$Y)
