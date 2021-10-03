#' ---
#' title: "Daily Prices to Monthly Returns"
#' author: "Juan Malaver"
#' date: "October 3, 2021"
#' output: github_document
#' ---
#'
#' This script collects daily price data for 5 assets and coverts to monthly
#' returns.
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
               col_rename = "monthly.returns") %>%
  pivot_wider(names_from = symbol, values_from = monthly.returns) %>%
  tk_xts()

## visualizing asset returns
#+ Monthly Log Returns
highchart(type = "stock") %>%
  hc_title(text = "Monthly Log Returns") %>%
  hc_add_series(asset_returns[, symbols[1]], name = symbols[1]) %>%
  hc_add_series(asset_returns[, symbols[2]], name = symbols[2]) %>%
  hc_add_series(asset_returns[, symbols[3]], name = symbols[3]) %>%
  hc_add_series(asset_returns[, symbols[4]], name = symbols[4]) %>%
  hc_add_series(asset_returns[, symbols[5]], name = symbols[5]) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)

## returns histogram function
hc_hist_fun <- function(n = 1, object, color){
  hc_hist <- hist(object[, symbols[n]],
                  breaks = 50,
                  plot = FALSE)
  
  hchart(hc_hist, color = color) %>%
    hc_title(text = paste(symbols[n],
                          "Log Returns Distribution",
                          sep = " ")) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = FALSE)
}

#+ SPY Log Returns Distribution
hc_hist_fun(1, asset_returns, "cornflowerblue")

#+ XLF Log Returns Distribution
hc_hist_fun(2, asset_returns, "green")

#+ XLE Log Returns Distribution
hc_hist_fun(3, asset_returns, "pink")

#+ XLK Log Returns Distribution
hc_hist_fun(4, asset_returns, "purple")

#+ XLV Log Returns Distribution
hc_hist_fun(5, asset_returns, "yellow")