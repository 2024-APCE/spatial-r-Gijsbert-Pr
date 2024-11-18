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
psych::pairs.panels(Anderson2007 %>% select(RES_LHU,BIOMASS,FIRE_FRQ,NMS,
                                            ,LF_N),
                    stars = T, ellipses = F)
psych::pairs.panels(Anderson2007std %>% select(RES_LHU,BIOMASS,FIRE_FRQ,NMS,
                                               ,LF_N),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
multreg_std<-lm(LF_N~RES_LHU+BIOMASS+FIRE_FRQ+NMS, data=Anderson2007std) 
summary(multreg_std) #Only BIOMASS significant in this flat model (No internal relations studied)

# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
Leaf_N_model<-'LF_N~BIOMASS+RES_LHU+FIRE_FRQ+NMS
              BIOMASS~FIRE_FRQ+RES_LHU
              NMS~FIRE_FRQ+RES_LHU'
Leaf_N_model
Leaf_N_fit<-lavaan::sem(Leaf_N_model, data=Anderson2007std)

# show the model results
summary(Leaf_N_fit, standardized=T, fit.measures=T, rsquare=T)

# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR
# Regressions show relations (numbers and significance)
# R^2 shows the proportion of variance explained by the model


# also explore the models as shown in fig 5b and 5c of the Anderson2007 paper
# so repeat the model for leaf P content
Leaf_P_model<-'LF_P~BIOMASS+RES_LHU+FIRE_FRQ+NMS
              BIOMASS~FIRE_FRQ+RES_LHU
              NMS~FIRE_FRQ+RES_LHU'
Leaf_P_model
Leaf_P_fit<-lavaan::sem(Leaf_P_model, data=Anderson2007std)
summary(Leaf_P_fit, standardized=T, fit.measures=T, rsquare=T)

