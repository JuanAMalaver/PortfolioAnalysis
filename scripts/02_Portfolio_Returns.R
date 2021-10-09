#' ---
#' title: "Portfolio Returns"
#' author: "Juan Malaver"
#' date: "October 6, 2021"
#' output: github_document
#' always_allow_html: true
#' ---
#'
#' This script collects daily price data for 5 assets and coverts to monthly
#' returns.
## loading libraries
library(tidyverse)
library(tidyquant)
library(timetk)
library(highcharter)

## storing tickers in vector
symbols <- c("SPY", "XLF", "XLE", "XLK", "XLV")

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
               col_rename = "monthly.returns")

## storing weights in vector
wts <- c(0.4, 0.15, 0.15, 0.15, 0.15)

## generating portfolio returns
portfolio_returns <- asset_returns %>%
  tq_portfolio(assets_col = symbol,
               returns_col = monthly.returns,
               weights = wts,
               col_rename = "returns",
               rebalance_on = "months") %>%
  tk_xts()

## visualizing portfolio returns
#+ Monthly Portfolio Returns
highchart(type = "stock") %>%
  hc_title(text = "Portfolio Monthly Returns") %>%
  hc_add_series(portfolio_returns$returns,
                name = "Rebalanced Monthly",
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)

## portfolio returns distribution
#+ Monthly Portfolio Returns Distribution
hc_portfolio <-
  hist(portfolio_returns$returns, breaks = 50, plot = FALSE)

hchart(hc_portfolio,
       color = "cornflowerblue",
       name = "Portfolio") %>%
  hc_title(text = "Portfolio Returns Distribution") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE)
