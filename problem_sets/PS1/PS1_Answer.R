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
#We're 90% confident that the true mean will fall within the interval of (93.96, 102.92).


#####################
# Problem 2
#####################
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
#Ho: u=u0;   Ha: u>u0.
t.test(y,mu=100,alterntive="greater",conf.level=0.95)
#I fail to reject the H0 because p-value = 0.557 > 0.05.
#The average IQ of the school does not significantly differ from 100 on average.

#####################
# Problem 3
#####################
#Import the data
expenditure <- read.table("expenditure.txt", header=T)
y3 <- c(1, 2, 1, 3, 4, 1, 1, 4, 2, 1, 3, 4, 3, 2, 1, 3, 4, 1, 2, 3, 1, 1, 2, 1, 1, 3, 4)

#Plot the relationships among Y, X1, X2, and X3.
ggplot(expenditure, aes(expenditure$X1, expenditure$Y))+
  geom_point()+
  labs(x="Personal Income",y="Expenditure",title="Figure 1:Personal Income and Educational Expenditure")
#The graph shows that as the per capita personal income increases, per capita expenditure on public education increases.

ggplot(expenditure, aes(expenditure$X2, expenditure$Y))+
  geom_point()+
  labs(x="People under 18 yrs old",y="Expenditure",title="Figure 2:People under 18 and Educational Expenditure")
#The graph does not show a clear relationship between the number of residents under 18 years of age and the per capita expenditure on public education.

ggplot(expenditure, aes(expenditure$X3, expenditure$Y))+
  geom_point()+
  labs(x="People in urban area",y="Expenditure",title="Figure 3:People in urban area and Educational Expenditure")
#The graph shows that as the number of people residing in urban areas increases, per capita expenditure on public education increases.

#Plot the relationship between Y and Region
boxplot(expenditure$Y~expenditure$Region)
# The west has the highest per cpita expenditure on public education.

#Plot the reltionship between Y and X1.
ggplot(expenditure, aes(x=expenditure$X1,y=expenditure$Y))+
  geom_point(aes(color=expenditure$Region))+
  scale_color_gradient(name="Region", low="blue", high="orange")+
  labs(x="Personal Income",y="Expenditure")
#People with higher income in the Northeast and West are more likely to spend more on public education.
#Whilst people in the North Central and South do not spend more on public education if they earn higher income. 