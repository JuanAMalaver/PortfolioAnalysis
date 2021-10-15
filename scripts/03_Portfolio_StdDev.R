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
  summarise(expected_return = mean(Ra)) %>%
  mutate(symbol = "Portfolio")

## generating groups for above and below one standard deviation
portfolio_groups <- portfolio_returns %>%
  mutate(group = ifelse(Ra > (portfolio_mean$expected_return + portfolio_stdev$Stdev) | (Ra < (portfolio_mean$expected_return - portfolio_stdev$Stdev)),
                        "Outlier",
                        "Normal")) %>%
  mutate(color = case_when(
    group == "Outlier" ~ "#f45b5b",
    group == "Normal" ~ "#7cb5ec"
  ))


## setting color palette
colors <- tibble(
  symbol = append(symbols, "Portfolio"),
  hex = c("#434348", "#90ed7d", "#f7a35c", "#8085e9", "#f15c80", "#7cb5ec")
) %>%
  arrange(symbol)

## binding rows
stdev_combined <- portfolio_stdev %>%
  mutate(symbol = "Portfolio") %>%
  bind_rows(asset_stdev) %>%
  arrange(desc(Stdev)) %>%
  inner_join(colors)

## calculating mean asset returns
mean_returns <- asset_returns %>%
  group_by(symbol) %>%
  summarise(expected_return = mean(Ra)) %>%
  bind_rows(portfolio_mean)

## joining means and stdevs
mean_stdev_combined <- stdev_combined %>%
  inner_join(mean_returns, by = "symbol")

## setting rolling window
window <- 24

## rolling standard deviation
portfolio_rolling_stdev <- portfolio_returns %>%
  tq_mutate(
    mutate_fun = rollapply,
    width = window,
    FUN = sd,
    col_rename = "rolling_sd"
  ) %>%
  select(date, rolling_sd) %>%
  drop_na()

## visualizing portfolio returns
#+ Monthly Portfolio Stdev
hchart(portfolio_groups, "scatter", hcaes(x = date, y = Ra, group = group, color = color)) %>%
  hc_title(text = "Portfolio Monthly Return Anomalies") %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = ""),
             plotLines = list(
               list(
                 dashStyle = "Dash",
                 color = "#999999",
                 width = 1.5,
                 value = portfolio_mean$expected_return + portfolio_stdev$Stdev,
                 zIndex = 1),
                list(
                 dashStyle = "Dash",
                 color = "#999999",
                 width = 1.5,
                 value = portfolio_mean$expected_return - portfolio_stdev$Stdev,
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
hchart(stdev_combined, "bar", hcaes(x = symbol, y = Stdev, color = hex)) %>%
  hc_title(text = "Asset and Portfolio Standard Deviation Comparison") %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "Monthly Return Standard Deviation")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE) %>%
  hc_tooltip(pointFormat = "Stdev: {point.y}")

## plotting mean returns against standard deviations
#+ Expected Monthly Returns versus Risk
hchart(mean_stdev_combined, "scatter", hcaes(x = Stdev,
                                             y = expected_return,
                                             color = hex,
                                             group = symbol)) %>%
  hc_title(text = "Expected Monthly Returns versus Risk") %>%
  hc_xAxis(title = list(text = "Expected Monthly Returns")) %>%
  hc_yAxis(title = list(text = "Monthly Return Standard Deviation")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_colors(colors$hex)

## plotting rolling standard deviation
#+ Rolling Volatility
highchart(type = "stock") %>%
  hc_title(text = "24-Month Rolling Volatility") %>%
  hc_add_series(portfolio_rolling_stdev %>% tk_xts(), color = "#7cb5ec") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_yAxis(labels = list(format = "{value}%"),
           opposite = FALSE) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE) %>%
  hc_tooltip(pointFormat = "Standard Deviation: {point.y}")

