# Packages ####

# forecasting
library(fable)
# time series plotting
library(feasts)
# time series data manipulation
library(tsibble)
# a different time series viz package
library(timetk)
# data manipulation
library(dplyr)
library(purrr)
library(tidyr)
# general viz
library(ggplot2)

elec <- readr::read_csv('data/electricity_france.csv')
elec

elec <- elec %>% 
    filter(Date >= as.Date('2007-01-01')) %>% 
    mutate(Year=lubridate::year(Date)) %>% 
    as_tsibble(index=Date)
elec
elec %>% class()

ts_elec <- ts(elec$ActivePower, start=min(elec$Date), end=max(elec$Date))
ts_elec
ts_elec %>% class

xts_elec <- xts::as.xts(ts_elec)
xts_elec %>% head
xts_elec %>% class()

# ts and xts for historical overview
# zoo: zeilles' ordered observations

# Visualization ####

# from feasts
elec %>% autoplot()
elec %>% autoplot(ActivePower)
elec %>% autoplot(ReactivePower)

elec %>% gg_season(ActivePower, period='year')

# from timetk

elec %>% 
    plot_time_series(.date_var=Date, .value=ActivePower)

elec %>% plot_time_series(.date_var=Date, .value=ActivePower, .color_var=Year)
elec %>% 
    plot_time_series(
        .date_var=Date, .value=ActivePower, .color_var=Year, 
        .facet_vars=Year, .facet_scales='free_x'
    )

elec %>% 
    plot_seasonal_diagnostics(Date, ActivePower)

# ACF ####

# autocorrelation function

elec %>% ACF(ActivePower)
elec %>% ACF(ActivePower) %>% autoplot()

# Simple Forecasting Models ####

# from fable
mean_mod <- elec %>% 
    model(Mean=MEAN(ActivePower))
mean_mod
mean_mod %>% class
mean_mod$Mean[[1]]

mean_mod %>% forecast(h=90)
mean_mod %>% forecast(h='90 days')
mean_mod %>% forecast(h='3 months')

mean_mod %>% forecast(h=90) %>% autoplot()
mean_mod %>% forecast(h=90) %>% autoplot(elec)

# naive method

naive_mod <- elec %>% 
    model(Naive=NAIVE(ActivePower))
naive_mod %>% forecast(h=90)
naive_mod %>% forecast(h=90) %>% autoplot(elec)

snaive_mod <- elec %>% 
    model(SNaive=SNAIVE(ActivePower ~ lag('month') + lag('year') + lag('week')))
snaive_mod %>% forecast(h=90) %>% autoplot(elec)

# Compare Models ####

simple_mods <- elec %>% 
    model(
        Mean=MEAN(ActivePower)
        , Naive=NAIVE(ActivePower)
        , SNaive=SNAIVE(ActivePower ~ lag('year'))
    )
simple_mods

simple_mods %>% forecast(h=90)
simple_mods %>% forecast(h=90) %>% tail()
simple_mods %>% forecast(h=90) %>% print(n=250)

simple_mods %>% forecast(h=90) %>% autoplot(elec, level=NULL)
simple_mods %>% forecast(h=90) %>% autoplot(elec)

# Transformations ####

# Log

elec %>% autoplot(ActivePower)
elec %>% autoplot(log(ActivePower))

# Box-Cox

elec %>% autoplot(box_cox(ActivePower, lambda=1.7))
elec %>% autoplot(box_cox(ActivePower, lambda=0.7))
elec %>% autoplot(box_cox(ActivePower, lambda=0.07))

# from feasts
best_lambda <- elec %>% 
    features(ActivePower, features=guerrero) %>% 
    pull(lambda_guerrero)
best_lambda

elec %>% autoplot(box_cox(ActivePower, lambda=best_lambda))
