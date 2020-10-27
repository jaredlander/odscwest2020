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

library(skimr)
skim(train)

# Feature Engineering ####

# from recipes

# caret
# tidymodels
# food themes
# Max Kuhn

# Outcomes: response, y, label, target, dependent variable, output, known, result
# inputs: predictors, x, features, covariates, independent variable, data, attributes, descriptors

table(credit_data$Status)

# red, green, blue
colors <- c('blue', 'blue', 'red', 'green', 'blue', 'green', 'blue', 'blue', 'blue', 'blue',
            'red', 'green', 'pink', 'yellow', 'pink', 'purple')
model.matrix(~ colors)
colors2 <- c('blue', 'blue', 'red', 'green', 'blue', 'green', 'blue', 'blue', 'blue', 'blue',
            'red', 'green', 'Misc', 'Misc', 'Misc', 'Misc')
model.matrix(~colors2)

rec1 <- recipe(Status ~ ., data=train) %>% 
    # xgboost can handle this, so we'll remove it later
    step_downsample(Status, under_ratio=1.2) %>% 
    # not really needed for xgboost
    step_normalize(Age, Price) %>% 
    # collapse infrequent columns in categorical variables
    # step_other(Home, Marital, Records, Job, other='Misc')
    # this line is a shortcut for the line above
    step_other(all_nominal(), -Status, other='Misc') %>% 
    # remove columns with very little variability, nero-zero variance
    step_nzv(all_predictors()) %>% 
    # xgboost doesn't need imputation, so we will remove later
    step_modeimpute(all_nominal(), -Status) %>% 
    step_knnimpute(all_numeric()) %>% 
    step_dummy(all_nominal(), -Status, one_hot=TRUE)
rec1
