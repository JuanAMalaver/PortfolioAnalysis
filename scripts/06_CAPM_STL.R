library(tidyverse)
library(tidyquant)
library(timetk)
library(tsibble)
library(feasts)
library(fable)
library(broom)

## storing tickers in vector
symbols <- c("AAPL",
             "MSFT",
             "GOOG",
             "GOOGL",
             "AMZN",
             "TSLA",
             "FB",
             "PYPL",
             "NVDA",
             "NFLX")

## retrieving stock prices
prices <- symbols %>%
  tq_get(get = "stock.prices",
         from = "2016-01-01")

prices <- tq_get("NVDA", get = "stock.prices", from = "2021-03-04", to = "2022-03-04")

## generating monthly returns
asset_returns <- prices %>%
  group_by(symbol) %>%
  tq_transmute(adjusted,
               periodReturn,
               period = "daily",
               type = "log",
               col_rename = "Ra")

## retrieving market returns
market_returns <- tq_get("SPY", get = "stock.prices", from = "2021-03-04", to = "2022-03-04") %>%
  tq_transmute(adjusted,
               periodReturn,
               period = "daily",
               type = "log",
               col_rename = "Rm")

## CAPM beta
asset_returns %>%
  left_join(market_returns, by = "date") %>%
  group_by(symbol) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(Ra ~ Rm, data = .))) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(tidy)

## 
asset_returns %>%
  left_join(market_returns, by = "date") %>%
  filter(symbol == "AAPL") %>%
  ggplot(aes(y = Ra, x = Rm)) +
  geom_point() + 
  geom_smooth(method = "lm")

## converting to tsibble
asset_returns_tsbl <- asset_returns %>%
  mutate(month = yearmonth(date)) %>%
  select(-date) %>%
  as_tsibble(key = symbol, index = month)

## STL decomposition
asset_returns_stl <- asset_returns_tsbl %>%
  model(stl = STL(Ra ~ trend(window = 7) + season(window = "periodic"),
        robust = TRUE))

## STL plot
asset_returns_stl %>%
  # filter(symbol == "AAPL") %>%
  components() %>%
  autoplot()

## bootstrap series
asset_returns_stl %>%
  filter(symbol == "AAPL") %>%
  generate(new_data = asset_returns_tsbl, times = 10,
           bootstrap_block_size = 8) %>%
  autoplot(.sim) +
  autolayer(asset_returns_tsbl, Ra) +
  guides(colour = "none") +
  labs(title = "Asset Returns: Bootstrapped series",
       y="Monthly Log Returns")
