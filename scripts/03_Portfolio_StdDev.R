#' ---
#' title: "Portfolio Standard Deviation"
#' author: "Juan Malaver"
#' date: "October 8, 2021"
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
               col_rename = "Ra")

## storing weights in vector
wts <- c(0.4, 0.15, 0.15, 0.15, 0.15)

## generating portfolio returns
portfolio_returns <- asset_returns %>%
  tq_portfolio(assets_col = symbol,
               returns_col = Ra,
               weights = wts,
               col_rename = "Ra",
               rebalance_on = "months")

## calculating standard deviation for underlying assets
asset_stdev <- asset_returns %>%
  tq_performance(Ra = Ra,
                 Rb = NULL,
                 performance_fun = table.Stats) %>%
  select(Stdev)

## calculating standard deviation for rebalanced portfolio
portfolio_stdev <- portfolio_returns %>%
  tq_performance(Ra = Ra,
                 Rb = NULL,
                 performance_fun = table.Stats) %>%
  select(Stdev)

## calculating portfolio mean returns
portfolio_mean <- portfolio_returns %>%
  summarise(mean = mean(Ra))

## generating groups for above and below one standard deviation
portfolio_groups <- portfolio_returns %>%
  mutate(group = ifelse(Ra > (portfolio_mean$mean + portfolio_stdev$Stdev),
                        "Above",
                        ifelse(Ra < (portfolio_mean$mean - portfolio_stdev$Stdev),
                               "Below",
                               "Between")))


## setting color palette
colors <- tibble(
  symbol = append(symbols, "Portfolio"),
  hex = c("#434348", "#90ed7d", "#f7a35c", "#8085e9", "#f15c80", "#7cb5ec")
)

## binding rows
stdev_combined <- portfolio_stdev %>%
  mutate(symbol = "Portfolio") %>%
  bind_rows(asset_stdev) %>%
  arrange(desc(Stdev)) %>%
  inner_join(colors)

## visualizing portfolio returns
#+ Monthly Portfolio Stdev
hchart(portfolio_groups, "scatter", hcaes(x = date, y = Ra, group = group)) %>%
  hc_title(text = "Portfolio Monthly Return Anomalies") %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = ""),
             plotLines = list(
               list(
                 dashStyle = "Dash",
                 color = "#999999",
                 width = 1.5,
                 value = portfolio_mean$mean + portfolio_stdev$Stdev,
                 zIndex = 1),
                list(
                 dashStyle = "Dash",
                 color = "#999999",
                 width = 1.5,
                 value = portfolio_mean$mean - portfolio_stdev$Stdev,
                 zIndex = 1)
               )
           ) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE) %>%
  hc_tooltip(pointFormat = "x: {point.x:%b \'%y} <br> y: {point.y}")

## visualizing portfolio and asset standard deviations
#+ Portfolio and Asset Stdev
hchart(stdev_combined, "column", hcaes(x = symbol, y = Stdev, color = hex)) %>%
  hc_title(text = "Asset and Portfolio Standard Deviation Comparison") %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Monthly Return Standard Deviation")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE) %>%
  hc_tooltip(pointFormat = "Stdev: {point.y}")
