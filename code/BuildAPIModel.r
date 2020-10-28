library(tidymodels)

data(credit_data, package='modeldata')

# build a feature engineering recipes
# build a model specification
# combine them into a workflow
# fit a model on the data
# save the trained model to disc

rec0 <- recipe(Status ~ Income + Seniority + Records + Amount + Job, data=credit_data) %>% 
    step_other(all_nominal(), -Status, other='Misc') %>% 
    step_nzv(all_predictors()) %>% 
    step_dummy(all_nominal(), -Status, one_hot=TRUE)

spec0 <- boost_tree(
    mode='classification'
    , trees=53
    , tree_depth=2
    , learn_rate=0.2
    , sample_size=0.52
) %>% 
    set_engine('xgboost')

rec0
spec0

flow0 <- workflow() %>% 
    add_recipe(rec0) %>% 
    add_model(spec0)
flow0

mod0 <- fit(flow0, data=credit_data)
mod0 %>% extract_model() %>% vip::vip()

readr::write_rds(mod0, file='code/mod0.rds')

loaded_mod <- readr::read_rds(file='code/mod0.rds')
loaded_mod %>% extract_model() %>% vip::vip()

data.frame(Income=156, Seniority=5, Records='yes', Amount=4500, Job='fixed') %>% 
    jsonlite::write_json(x=., path='one_row.json')
