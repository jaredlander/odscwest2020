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

cat_train_1 <- c('rent', 'own', 'mortgage')
cat_test_1 <- c('rent', 'parents')

cat_train_2 <- c('rent', 'own', 'mortgage')
cat_test_2 <- c('rent', 'own')

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

# Model Specification ####

# from parsnip

xg_spec1 <- boost_tree(mode='classification') %>% 
    set_engine('xgboost')

xg_spec1

boost_tree(mode='classification') %>% set_engine('C5.0')
boost_tree(mode='classification') %>% set_engine('spark')

# BART: dbart
# catboost
# LightGBM

xg_spec1 <- boost_tree(mode='classification', trees=100) %>% 
    set_engine('xgboost')
xg_spec1

# gives us a uniform naming convention for all of the parameters

linear_reg() %>% set_engine('lm')
linear_reg(penalty=0.826) %>% set_engine('glmnet')
linear_reg() %>% set_engine('keras')
linear_reg() %>% set_engine('stan')
linear_reg() %>% set_engine('spark')

rand_forest() %>% set_engine('randomForest')
rand_forest() %>% set_engine('ranger')

# Build Workflow ####

rec1 %>% prep()

prepped <- rec1 %>% prep() %>% bake(new_data=NULL)
prepped

fit0 <- fit(xg_spec1, Status ~ ., data=prepped)
fit0

# from workflows

# combine featuring engineering and model specification into one step

flow1 <- workflow() %>% 
    add_recipe(rec1) %>% 
    add_model(xg_spec1)
flow1


