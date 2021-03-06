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

rec_prep <- rec1 %>% prep()
bake(rec_prep, new_data=NULL)
bake(rec_prep, new_data=test)
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

# Fit Our Model

fit1 <- fit(flow1, data=train)
# fit1 <- fit(flow1, data=train2)
fit1
fit1 %>% class()

fit1 %>% extract_model() %>% class()
fit1 %>% extract_model() %>% vip()
fit1 %>% extract_model() %>% xgboost::xgb.plot.multi.trees()

# readr::write_rds(fit1, 'fit1.rds')
# xgboost::xgb.save(fit1 %>% extract_model(), fname='xg1.model')

# How did we Do? ####

# accuracy, logloss, AUC

# from yardstick

loss_fn <- metric_set(accuracy, mn_log_loss, roc_auc)
loss_fn

# train and validation sets
# cross-validation

# from rsample
val_split <- validation_split(data=train, prop=0.8, strata='Status')
val_split
val_split$splits[[1]]

credit_split

credit_split %>% class()
val_split$splits[[1]] %>% class()

val_split %>% class()

# from tune
val1 <- fit_resamples(object=flow1, resamples=val_split, metrics=loss_fn)

val1
val1 %>% collect_metrics()
val1$.metrics

library(animation)
cv.ani(k=10)

# from rsample
cv_split <- vfold_cv(data=train, v=10, strata='Status')
cv_split
cv_split %>% class()
val_split %>% class()
cv_split$splits[[1]]

vfold_cv(data=train, v=10, strata='Status', repeats=3)
cv_split <- vfold_cv(data=train, v=5, strata='Status', repeats=2)
cv_split

val1 <- fit_resamples(object=flow1, resamples=val_split, metrics=loss_fn)
cv1 <- fit_resamples(object=flow1, resamples=cv_split, metrics=loss_fn)

cv1
cv1$.metrics[[1]]
cv1$.metrics[[2]]
cv1$.metrics[[3]]

cv1 %>% collect_metrics()

# More Parameters ####

xg_spec2 <- boost_tree(mode='classification', trees=300) %>% 
    set_engine('xgboost')
xg_spec2

# workflow() %>% 
#     add_model(xg_spec2) %>% 
#     add_recipe(rec1)

flow2 <- flow1 %>% 
    update_model(xg_spec2)
flow2

val2 <- fit_resamples(flow2, resamples=val_split, metrics=loss_fn)
val2
val2 %>% collect_metrics()

xg_spec3 <- boost_tree('classification', trees=300, learn_rate=0.2) %>% 
    set_engine('xgboost')
xg_spec3

flow3 <- flow2 %>% 
    update_model(xg_spec3)

val3 <- fit_resamples(flow3, resamples=val_split, metrics=loss_fn)
val3 %>% collect_metrics()

xg_spec4 <- boost_tree('classification', trees=300, learn_rate=0.2, sample_size=0.5) %>% 
    set_engine('xgboost')
xg_spec4

flow4 <- flow3 %>% 
    update_model(xg_spec4)
val4 <- fit_resamples(flow4, resamples=val_split, metrics=loss_fn)
val4 %>% collect_metrics()

# Missing Data ####

rec2 <- recipe(Status ~ ., data=train) %>% 
    step_nzv(all_predictors()) %>% 
    step_other(all_nominal(), -Status, other='Misc') %>% 
    themis::step_downsample(Status, under_ratio=1.2) %>% 
    step_dummy(all_nominal(), -Status, one_hot=TRUE)
rec2

flow5 <- flow4 %>% 
    update_recipe(rec2)
flow5

val5 <- fit_resamples(flow5, resamples=val_split, metrics=loss_fn)
val5 %>% collect_metrics()
val4 %>% collect_metrics()
val5
val5$.notes

# Imbalanced Data

rec3 <- recipe(Status ~ ., data=train) %>% 
    step_nzv(all_predictors()) %>% 
    step_other(all_nominal(), -Status, other='Misc') %>% 
    step_dummy(all_nominal(), -Status, one_hot=TRUE)
rec3

flow6 <- flow5 %>% 
    update_recipe(rec3)

val6 <- fit_resamples(flow6, resamples=val_split, metrics=loss_fn)
val5 %>% collect_metrics()
val6 %>% collect_metrics()

table(train$Status)
1004/2561

scaler <- train %>% count(Status) %>% pull(n) %>% purrr::reduce(`/`)

xg_spec5 <- boost_tree('classification', trees=300, learn_rate=0.2, sample_size=0.5) %>% 
    set_engine('xgboost', scale_pos_weight=!!(1/scaler))
xg_spec5

flow7 <- flow6 %>% 
    update_model(xg_spec5)
flow7

val7 <- fit_resamples(flow7, resamples=val_split, metrics=loss_fn)
val7 %>% collect_metrics()
val6 %>% collect_metrics()
val5 %>% collect_metrics()

# Tune Parameters ####

# from tune

xg_spec6 <- boost_tree('classification', learn_rate=0.2, sample_size=0.5, trees=tune()) %>% 
    set_engine('xgboost', scale_pos_weight=!!(1/scaler))
xg_spec6

flow8 <- flow7 %>% 
    update_model(xg_spec6)
flow8

# does not work
# fit8 <- fit(flow8, data=train)

# does not work
val8 <- fit_resamples(flow8, resamples=val_split, metrics=loss_fn)
val8$.notes

# benefits and draw backs of validate vs cross-validate

# from doFuture and parallel
registerDoFuture()
cl <- makeCluster(6)
plan(cluster, workers=cl)

options(tidymodels.dark=TRUE)
# from tictoc
tic()
# tune
tune8_val <- tune_grid(
    flow8,
    resamples=val_split,
    grid=20,
    metrics=loss_fn,
    control=control_grid(verbose=TRUE, allow_par=TRUE)
)
toc()

tune8_val
tune8_val$.notes
tune8_val$.metrics
tune8_val %>% collect_metrics()

tune8_val %>% show_best(metric='roc_auc')

tic()
tune8_cv <- tune_grid(
    flow8,
    resamples=cv_split,
    grid=20,
    metrics=loss_fn,
    control=control_grid(verbose=TRUE, allow_par=TRUE)
)
toc()

tune8_cv
tune8_cv$.metrics[[1]]$trees %>% unique

tune8_cv %>% collect_metrics()
tune8_cv %>% autoplot()
tune8_cv %>% show_best(metric='roc_auc')

# Other Tuning Parameters ####

xg_spec7 <- boost_tree(
    'classification',
    trees=tune(), learn_rate=0.2, sample_size=tune(), tree_depth=tune()
) %>% 
    set_engine('xgboost', scale_pos_weight=!!(1/scaler))
xg_spec7

flow9 <- flow8 %>% 
    update_model(xg_spec7)
flow9

flow9 %>% parameters()
flow9 %>% parameters() %>% class()
flow9 %>% parameters() %>% pull(object)

# from dials
trees()
trees(range=c(10, 300))

tree_depth()
tree_depth(range=c(2, 8))

sample_size()
# sample_size(range=c(0.3, 0.8))
sample_prop()
sample_prop(c(0.3, 0.8))

params9 <- flow9 %>% 
    parameters() %>% 
    update(
        trees=trees(range=c(10, 300)),
        tree_depth=tree_depth(range=c(2, 8)),
        sample_size=sample_prop(range=c(0.3, 0.8))
    )
params9
params9 %>% pull(object)

tic()
val9 <- tune_grid(
    flow9,
    resamples=val_split,
    grid=40,
    metrics=loss_fn,
    control=control_grid(verbose=TRUE, allow_par=TRUE),
    param_info=params9
)
toc()
val9

val9 %>% show_best(metric='roc_auc')
val9 %>% autoplot(metric='roc_auc')


grid10 <- grid_max_entropy(params9, size=40)
grid10

tic()
val10 <- tune_grid(
    flow9,
    resamples=val_split, # or cv_split
    grid=grid10, 
    metrics=loss_fn,
    control=control_grid(verbose=TRUE, allow_par=FALSE)
)
toc()

val10 %>% collect_metrics()
val10 %>% show_best(metric='roc_auc', n=10)

val10 %>% select_best(metric='roc_auc')

boost_tree('classification', trees=127, tree_depth=2, sample_size=0.509)

# Finalize Model ####

mod10 <- flow9 %>% 
    finalize_workflow(val10 %>% select_best(metric='roc_auc'))
flow9
mod10

val10.1 <- fit_resamples(mod10, resamples=val_split, metrics=loss_fn)
val10.1 %>% collect_metrics()
val10 %>% show_best()
val10.1 %>% collect_metrics()

test

# Last Fit ####

results10 <- last_fit(mod10, split=credit_split, metrics=loss_fn)
results10 %>% collect_metrics()

# Make Predictions ####

# fit the model on ALL the data
# predict on some new data (pretend 'test' is new)

fit10 <- fit(mod10, data=credit_data)

fit10 %>% extract_model() %>% vip()

# pretend 'test' is new
preds10 <- predict(fit10, new_data=test)
preds10

# fit is for fitting one model with set parameters
# fit_resamples is for fitting multiple models for validation with set parameters
# tune_grid is for tuning over tuning parameters

preds10_prob <- predict(fit10, new_data=test, type='prob')
preds10_prob
