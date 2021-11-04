#' ---
#' title: "Portfolio Skewness"
#' author: "Juan Malaver"
#' date: "November 3, 2021"
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

## calculating skewness for underlying assets
asset_skew <- asset_returns %>%
  tq_performance(Ra = Ra,
                 Rb = NULL,
                 performance_fun = table.Stats) %>%
  select(Skewness)

## calculating skewness for rebalanced portfolio
portfolio_skew <- portfolio_returns %>%
  tq_performance(Ra = Ra,
                 Rb = NULL,
                 performance_fun = table.Stats) %>%
  select(Skewness)

# ## generating groups for above and below one standard deviation
# portfolio_returns <- portfolio_returns %>%
#   mutate(group = ifelse(Ra > (mean(Ra) + 2*sd(Ra)) | (Ra < (mean(Ra) - 2*sd(Ra))),
#                         "Outlier",
#                         "Normal")) %>%
#   mutate(color = case_when(
#     group == "Outlier" ~ "#f45b5b",
#     group == "Normal" ~ "#7cb5ec"
#   ))


## plotting portofilio return density with mean and median
hchart(density(portfolio_returns$Ra), "line", name = "Rebalanced Portfolio", color = "#2f7ed8") %>%
  hc_title(text = "Portfolio Return Density Skewness") %>%
  hc_xAxis(title = list(text = "Monthly Log Returns"),
           plotLines = list(
             list(
               label = list(text = "mean"),
               color = "#FF0000",
               width = 2,
               dashStyle = "Dot",
               value = mean(portfolio_returns$Ra),
               zIndex = 1
             ),
             list(
               label = list(text = "median"),
               width = 2,
               dashStyle = "Dot",
               value = median(portfolio_returns$Ra),
               zIndex = 1
             )
           ),
           plotBands = list(
             list(
               from = min(portfolio_returns$Ra),
               to = mean(portfolio_returns$Ra),
               color = hex_to_rgba("red", 0.1),
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

colors <- tibble(
  symbol = append(symbols, "Portfolio"),
  hex = c("#434348", "#90ed7d", "#f7a35c", "#8085e9", "#f15c80", "#7cb5ec",
          "#e4d354", "#2b908f", "#f45b5b", "#91e8e1", "#2f7ed8")
) %>%
  arrange(symbol)

skew_combined <- portfolio_skew %>%
  mutate(symbol = "Portfolio") %>%
  bind_rows(asset_skew) %>%
  arrange(desc(Skewness)) %>%
  inner_join(colors)

## bar chart comparing skewness
hchart(skew_combined, "bar", hcaes(x = symbol, y = Skewness, color = hex)) %>%
  hc_title(text = "Asset and Portfolio Skewness Comparison") %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Monthly Return Skewness")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE) %>%
  hc_tooltip(pointFormat = "Skewness: {point.y}")

window <- 24

portfolio_rolling_skew <- portfolio_returns %>%
  tq_mutate(
    select = Ra,
    mutate_fun = rollapply,
    width = window,
    FUN = skewness,
    col_rename = "rolling_skew"
  ) %>%
  select(date, rolling_skew) %>%
  drop_na() %>%
  tk_xts()

## rolling skewness
highchart(type = "stock") %>%
  hc_title(text = "Portfolio Rolling Skewness") %>%
  hc_add_series(portfolio_rolling_skew,
                name = "Rebalanced Portfolio",
                color = "#2f7ed8") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_yAxis(labels = list(format = "{value}%"),
           opposite = FALSE) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(pointFormat = "Skewness: {point.y}")
