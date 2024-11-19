#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of woody cover
# Paper:
# browseURL("https://docs.google.com/spreadsheets/d/1C4Et19BZJB4kQEtOSHaiSxYB-SjHYz4QAJH7BwOgztE/edit?gid=0#gid=0")

# restore libraries

rm(list = ls()) # clear environment

library(tidyverse)
# load the lavaan library
library(lavaan)

# key variables of interest: 
# predictors:
# ALL_LHU - total large herbivore density
# RES_LHU - density resident herbivores 
# FIRE_FRQ - fire frequency
# NMS - plant species composition (NMDS ordination axis score)
# response: 
# LF_N - plant leaf nitrogen content
# other variables:
# PRECIP - annual rainfall
# THETRI - biomass of Themada triandra, a tall grass positively responding to fire
# BIOMASS - total aboveground plant biomass
# SOIL_RN - total soil reactive nitrogen (ammonium+nitrate)
# LF_NA - plant leaf sodium content

# dataset:
#browseURL("https://docs.google.com/spreadsheets/d/1C4Et19BZJB4kQEtOSHaiSxYB-SjHYz4QAJH7BwOgztE/edit?gid=0#gid=0")
# read the data from the google docs link:
SEM_data<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS7xJtSN92rte6HHjzPxf9n3I_cVI4cshpItca4czd0iOHcuPJXil842MZ0xMHnS5Y5u9RpftaJZPlk/pub?gid=1502613483&single=true&output=csv")   # total soil reactive nitrogen
names(SEM_data)
# standardize all variables to mean 0 and standard deviation 1
SEM_data_std <- SEM_data |>
  mutate_all(~(scale(.) %>% as.vector)) |> #Change unit axis but not the relationship between variables
  as_tibble()
SEM_data_std
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEM_data %>% select(elevation,rainfall,cec,burnfreq,distancetoriver
                                            ,woody),
                    stars = T, ellipses = F)
psych::pairs.panels(SEM_data_std %>% select(elevation,rainfall,cec,burnfreq,distancetoriver
                                               ,woody),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
multreg_std<-lm(woody~elevation+rainfall+cec+burnfreq+distancetoriver, data=SEM_data_std) 
summary(multreg_std) #Only BIOMASS significant in this flat model (No internal relations studied)

# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

#01 Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
Woody_model1<-'woody~burnfreq+distancetoriver+rainfall+cec
              rainfall~elevation
              burnfreq~rainfall+elevation+distancetoriver+cec'
Woody_fit1<-lavaan::sem(Woody_model1, data=SEM_data_std)

# show the model results
summary(Woody_fit1, standardized=T, fit.measures=T, rsquare=T)

# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR
# Regressions show relations (numbers and significance)
# R^2 shows the proportion of variance explained by the model


#02 Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
#Hypothesis: Woody cover is mediated by rainfall and fire frequency, while CEC and elevation indirectly influence woody cover via these mediators.
Woody_model2 <- 'woody~burnfreq+rainfall
                rainfall~cec+elevation+distancetoriver
                burnfreq~rainfall+elevation+distancetoriver'
Woody_fit2<-lavaan::sem(Woody_model2, data=SEM_data_std)
summary(Woody_fit2, standardized=T, fit.measures=T, rsquare=T)

#03 Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model
#Hypothesis: Direct effects of elevation and rainfall on woody cover, with indirect effects of distance to rivers and fire frequency.
Woody_model3 <- 'woody ~ rainfall + elevation + burnfreq
                burnfreq ~ rainfall + distancetoriver
                rainfall ~ cec + elevation + distancetoriver'
Woody_fit3<-lavaan::sem(Woody_model3, data=SEM_data_std)
summary(Woody_fit3, standardized=T, fit.measures=T, rsquare=T)

#04 Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model
#Hypothesis: Rainfall and soil fertility (CEC) mediate the effect of elevation and distance to rivers on woody cover
Woody_model4 <- 'woody ~ rainfall + burnfreq + cec
                rainfall ~ elevation + distancetoriver + cec
                burnfreq ~ rainfall + elevation + distancetoriver'
Woody_fit4<-lavaan::sem(Woody_model4, data=SEM_data_std)
summary(Woody_fit4, standardized=T, fit.measures=T, rsquare=T)

#05 Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model
#Hypothesis: Fire frequency acts as a central mediator, influenced by multiple factors and affecting woody cover directly.
Woody_model5 <- 'woody ~ burnfreq + rainfall + cec
                burnfreq ~ rainfall + elevation + distancetoriver + cec
                rainfall ~ elevation + cec '
Woody_fit5<-lavaan::sem(Woody_model5, data=SEM_data_std)
summary(Woody_fit5, standardized=T, fit.measures=T, rsquare=T)

models <- list(Woody_fit1, Woody_fit2, Woody_fit3, Woody_fit4, Woody_fit5)
sapply(models, AIC)

