#' ---
#' title: "Portfolio Sharpe Ratio"
#' author: "Juan Malaver"
#' date: "November 30, 2021"
#' output: github_document
#' always_allow_html: true
#' ---
#'
#' This script collects daily prices for 5 assets and calculates standard
#' deviation of returns.
## loading libraries
library(tidyverse)
library(tidyquant)
library(timetk)
library(highcharter)

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

## generating monthly returns
asset_returns <- prices %>%
  group_by(symbol) %>%
  tq_transmute(adjusted,
               periodReturn,
               period = "monthly",
               type = "log",
               col_rename = "Ra")

## storing weights in vector
wts <- c(0.15, 0.15, 0.075, 0.075, 0.15, 0.15, 0.10, 0.05, 0.05, 0.05)

## generating portfolio returns
portfolio_returns <- asset_returns %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Ra,
               weights = wts,
               col_rename = "Ra",
               rebalance_on = "months")

## setting risk-free rate
rfr <- 0.0003

## calculating sharpe ratio for underlying assets
asset_sharpe <- asset_returns %>%
  tq_performance(Ra = Ra,
                 performance_fun = SharpeRatio,
                 Rf = rfr,
                 FUN = "StdDev") %>%
  rename(sharpe_ratio = `StdDevSharpe(Rf=0%,p=95%)`)

## calculating sharpe ratio for rebalanced portfolio
portfolio_sharpe <- portfolio_returns %>%
  tq_performance(Ra = Ra,
                 performance_fun = SharpeRatio,
                 Rf = rfr,
                 FUN = "StdDev") %>%
  rename(sharpe_ratio = `StdDevSharpe(Rf=0%,p=95%)`)

## generating groups for above and below risk-free rate
portfolio_groups <- portfolio_returns %>%
  mutate(ratio = mean(Ra - rfr)/sd(Ra - rfr)) %>%
  mutate(group = ifelse(
    Ra > rfr,
    "Above",
    "Below")
  ) %>%
  mutate(color = case_when(
    group == "Above" ~ "#90ed7d",
    group == "Below" ~ "#f45b5b"
  ))

## plotting returns around risk-free rate
hchart(portfolio_groups, "scatter", hcaes(x = date, y = Ra, group = group,
                                          color = color)) %>%
  hc_title(text = "Portfolio Returns Around Risk Free Rate") %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(labels = list(format = "{value}%"),
           title = list(text = "Monthly Log Returns"),
           plotLines = list(
             list(
               dashStyle = "Dot",
               color = "#999999",
               width = 1.5,
               value = rfr,
               zIndex = 1)
           )
  ) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE) %>%
  hc_tooltip(pointFormat = "Date: {point.x:%b \'%y} <br> Return: {point.y}%",
             valueDecimals = 3)


## plotting portfolio return density with risk-free rate
hchart(density(portfolio_returns$Ra), "line", name = "Rebalanced Portfolio", color = "#2f7ed8") %>%
  hc_title(text = "Portfolio Return Density") %>%
  hc_xAxis(title = list(text = "Monthly Log Returns"),
           plotLines = list(
             list(
               label = list(text = "Risk Free Rate"),
               color = "#FF0000",
               width = 2,
               dashStyle = "Dot",
               value = rfr,
               zIndex = 1
             )
           )
  ) %>%
  hc_yAxis(title = list(text = "Density")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE)

## specifying color palette
colors <- tibble(
  symbol = append(symbols, "Portfolio"),
  hex = c("#434348", "#90ed7d", "#f7a35c", "#8085e9", "#f15c80", "#7cb5ec",
          "#e4d354", "#2b908f", "#f45b5b", "#91e8e1", "#2f7ed8")
) %>%
  arrange(symbol)

## joining asset and portfolio sharpe ratios with colors
sharpe_combined <- portfolio_sharpe %>%
  mutate(symbol = "Portfolio") %>%
  bind_rows(asset_sharpe) %>%
  arrange(desc(sharpe_ratio)) %>%
  inner_join(colors)

## calculating portfolio return standard deviation
portfolio_stdev <- portfolio_returns %>%
  summarise(std_dev = sd(Ra)) %>%
  mutate(symbol = "Portfolio")

## calculating asset return standard deviation
stdev_combined <- asset_returns %>%
  group_by(symbol) %>%
  summarise(std_dev= sd(Ra)) %>%
  bind_rows(portfolio_stdev)

## joining sharpe ratios with standard deviation
sharpe_stdev_combined <- sharpe_combined %>%
  inner_join(stdev_combined, by = "symbol")

## plotting scatter of std dev vs sharpe ratio
hchart(sharpe_stdev_combined, "scatter", hcaes(x = round(std_dev, 3),
                                             y = sharpe_ratio,
                                             color = hex,
                                             group = symbol)) %>%
  hc_title(text = "Sharpe Ratio vs Standard Deviation") %>%
  hc_xAxis(title = list(text ="Monthly Return Standard Deviation")) %>%
  hc_yAxis(title = list(text = "Sharpe Ratio")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_colors(colors$hex) %>%
  hc_tooltip(pointFormat = "Standard Deviation: {point.x} <br> Sharpe Ratio: {point.y}",
             valueDecimals = 3)

## setting rolling window
window <- 24

## custom sharpe ratio function
sharpe_roll <- function(df) {
  SharpeRatio(
    df,
    Rf = rfr,
    FUN = "StdDev"
  )
}

## calculcating rolling sharpe ratio
portfolio_rolling_sharpe <- portfolio_returns %>%
  tq_mutate(
    select = Ra,
    mutate_fun = rollapply,
    width = window,
    align = "right",
    FUN = sharpe_roll,
    col_rename = "rolling_sharpe"
  ) %>%
  select(date, rolling_sharpe) %>%
  drop_na()

## converting to xts object
portfolio_rolling_sharpe_xts <- portfolio_rolling_sharpe %>%
  tk_xts()

## plotting rolling sharpe ratio
highchart(type = "stock") %>%
  hc_title(text = "Portfolio Rolling Sharpe Ratio") %>%
  hc_add_series(portfolio_rolling_sharpe_xts,
                name = "Rebalanced Portfolio",
                color = "#2f7ed8") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_yAxis(labels = list(format = "{value}"),
           opposite = FALSE) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(pointFormat = "Sharpe Ratio: {point.y}")
