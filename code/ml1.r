# Packages ####

# resampling, splitting and validation
library(rsample)
# feature engineering or preprocessing
library(recipes)
# specifying models
library(parsnip)
# tuning
library(tune)
# tuning parameters
library(dials)
# performance measurement
library(yardstick)
# variable importance plots
library(vip)
# combing feature engineering and model specification
library(workflows)
# data manipulation
library(dplyr)
# plotting
library(ggplot2)
# library(tidymodels)

# parallelism
library(doFuture)
library(parallel)
# timing
library(tictoc)

# Data ####

data(credit_data, package='modeldata')
credit_data <- credit_data %>% as_tibble()
credit_data

# EDA ####

ggplot(credit_data, aes(x=Status)) + geom_bar()
ggplot(credit_data, aes(x=Status, y=Amount)) + geom_violin()
ggplot(credit_data, aes(x=Status, y=Age)) + geom_violin()
ggplot(credit_data, aes(x=Status, y=Income)) + geom_violin(draw_quantiles=0.5)

ggplot(credit_data, aes(x=Age, y=Income, color=Status)) + geom_point()

ggplot(credit_data, aes(x=Age, y=Income, color=Status)) + 
    geom_hex() + 
    facet_wrap(~Status) + theme(legend.position='none')

# Split Data ####

set.seed(871)

# from rsample
credit_split <- initial_split(credit_data, prop=0.8, strata='Status')
credit_split
credit_split %>% class()

train <- training(credit_split)
test <- testing(credit_split)

train
train %>% glimpse()
train %>% class()
