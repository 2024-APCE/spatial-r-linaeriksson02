#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of Woody Cover
# Paper:
# browseURL("https://docs.google.com/spreadsheets/d/1Lt7tkF0jlhDziYL5CUI58FvpNM7xnm4Vba9VGXCI5hE/edit?gid=0#gid=0")

# restore libraries

rm(list = ls()) # clear environment

renv::restore()

library(tidyverse)
# load the lavaan library
library(lavaan)



# dataset:
# browseURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vSSsnkYCf5IHC7S4PCODOjp-fceBF7vYrDWt91_h8rltu5ULnu9QDbZHIhI_Smc8iE6M7_7gBqFm79k/pub?gid=744190039&single=true&output=csv")
# read the data from the google docs link:
SEM_data<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSSsnkYCf5IHC7S4PCODOjp-fceBF7vYrDWt91_h8rltu5ULnu9QDbZHIhI_Smc8iE6M7_7gBqFm79k/pub?gid=744190039&single=true&output=csv") 

SEM_data

# standardize all variables to mean 0 and standard deviation 1
# beacuse some variables are measured on the longer scale than others
SEM_data_std <- SEM_data |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()

SEM_data_std


################### next

SEM_model <- 'woody~burnfreq+rainfall
burnfreq~rainfall+CorProtAr
rainfall~elevation+hills
hills~~elevation
CorProtAr~elevation+hills'
SEM_model

SEM_fit <- lavaan::sem(SEM_model, data = SEM_data)
# show the model results
summary(SEM_fit, standardized = T, fit.measures = T, rsquare = T)


# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR

#### model 2
SEM_model2 <- 'woody~burnfreq+cec
cec~burnfreq+CorProtAr+rainfall
burnfreq~CorProtAr+rainfall
CorProtAr~elevation+hills
rainfall~elevation+hills
hills~~elevation'
SEM_model2

SEM_fit2 <- lavaan::sem(SEM_model2, data = SEM_data)
# show the model results
summary(SEM_fit2, standardized = T, fit.measures = T, rsquare = T)

AIC(SEM_fit, SEM_fit2)


#### model 3

SEM_model3 <- 'woody~cec+elevation+ burnfreq
cec~burnfreq+elevation
burnfreq~CorProtAr+rainfall
CorProtAr~elevation
rainfall~elevation'

SEM_model3

SEM_fit3 <- lavaan::sem(SEM_model3, data = SEM_data)
# show the model results
summary(SEM_fit3, standardized = T, fit.measures = T, rsquare = T)

AIC(SEM_fit, SEM_fit2, SEM_fit3)
