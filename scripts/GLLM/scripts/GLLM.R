remove(list=ls())
library(tidyverse)
library(lme4)
library(ggplot2)
CS<-read.table("C:/Users/praam/Documents/Master/APCE/CS_Lauwersmeer.csv",header=T, sep=",")|>
  na.omit(CS)|>
  as_tibble()

#check for outliers, for example:
hist(CS$CS)
#### step 1. linear regression ###
m1<-lm(CS~annual_density, data=CS)
summary(m1)
# Visualizing regression:
ggplot(CS, aes(x=annual_density, y=CS)) +
  geom_point(shape=1) + # Use hollow circles
  geom_smooth(method=lm, # Add linear regression line
              se=FALSE)
# check residuals
par(mfrow=c(2,2))
plot(m1) #looks good
hist(residuals(m1))

#Step 2: linear regression - density as factor 
#IF you are interested in the levels and their differences
## It gets you more degrees of freedom (reduces power)
CS$annual_density_cat<-as.factor(CS$annual_density_cat)
m2<-lm(CS~annual_density_cat, data=CS)
summary(m2) #intercept is the first value

#step 3 linear regression - taking annual means
CS2<- CS |>
  dplyr::group_by(annual_density) |>
  dplyr::summarize(CS_avg=mean(CS),
                   CS_sd=sd(CS),
                   n_obs=n(),
                   CS_se=CS_sd/sqrt(n_obs))
m3<-lm(CS_avg~annual_density, data=CS2)
summary(m3) 
#standard errror is higher than in the previous model because the sample size is a lot smaller
#visualisation
ggplot(CS2, aes(x=annual_density, y=CS_avg)) +
  geom_errorbar(aes(ymin=CS_avg-CS_se, ymax=CS_avg+CS_se), width=.1) +
  geom_point()+ geom_smooth(method=lm, # Add linear regression line
                            se=FALSE)
