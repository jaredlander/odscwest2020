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

ts_elec <- ts(elec$ActivePower, start=min(elec$Date), end=max(elec$Date))
ts_elec
ts_elec %>% class

xts_elec <- xts::as.xts(ts_elec)
xts_elec %>% head
xts_elec %>% class()

# ts and xts for historical overview
# zoo: zeilles' ordered observations

# Visualization ####

elec %>% autoplot()
elec %>% autoplot(ActivePower)
elec %>% autoplot(ReactivePower)
