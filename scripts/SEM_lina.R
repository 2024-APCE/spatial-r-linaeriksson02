#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of Woody Cover
# Paper:
# browseURL("https://docs.google.com/spreadsheets/d/1Lt7tkF0jlhDziYL5CUI58FvpNM7xnm4Vba9VGXCI5hE/edit?gid=0#gid=0")

# restore libraries

rm(list = ls()) # clear environment

library(tidyverse)
# load the lavaan library
library(lavaan)



# dataset:
browseURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vSOHGtB3_Ok0Zt4Afq4kfXC5j8RpBp_1lvIzzr6C1glSDAGlmYElHQ76s5HDWAPw85nLQ9v1MHMTBPG/pub?gid=2045022995&single=true&output=csv")
# read the data from the google docs link:
SEM_data<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSOHGtB3_Ok0Zt4Afq4kfXC5j8RpBp_1lvIzzr6C1glSDAGlmYElHQ76s5HDWAPw85nLQ9v1MHMTBPG/pub?gid=2045022995&single=true&output=csv") 

SEM_data

# standardize all variables to mean 0 and standard deviation 1
# beacuse some variables are measured on the longer scale than others
SEM_data_std <- SEM_data |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()

SEM_data_std


