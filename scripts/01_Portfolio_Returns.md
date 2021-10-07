Portfolio Returns
================
Juan Malaver
October 6, 2021

This script collects daily price data for 5 assets and coverts to
monthly returns.

``` r
## loading libraries
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(tidyquant)
```

    ## Loading required package: lubridate

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    ## Loading required package: PerformanceAnalytics

    ## Loading required package: xts

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

    ## 
    ## Attaching package: 'PerformanceAnalytics'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     legend

    ## Loading required package: quantmod

    ## Loading required package: TTR

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## == Need to Learn tidyquant? ====================================================
    ## Business Science offers a 1-hour course - Learning Lab #9: Performance Analysis & Portfolio Optimization with tidyquant!
    ## </> Learn more at: https://university.business-science.io/p/learning-labs-pro </>

``` r
library(timetk)
library(highcharter)

## storing tickers in vector
symbols <- c("SPY", "XLF", "XLE", "XLK", "XLV")

## retrieving stock prices
prices <- symbols %>%
  tq_get(get = "stock.prices",
         from = "2016-01-01")
```

    ## Warning: `type_convert()` only converts columns of type 'character'.
    ## - `df` has no columns of type 'character'

    ## Warning: `type_convert()` only converts columns of type 'character'.
    ## - `df` has no columns of type 'character'

    ## Warning: `type_convert()` only converts columns of type 'character'.
    ## - `df` has no columns of type 'character'

    ## Warning: `type_convert()` only converts columns of type 'character'.
    ## - `df` has no columns of type 'character'

    ## Warning: `type_convert()` only converts columns of type 'character'.
    ## - `df` has no columns of type 'character'

``` r
## generating monthly returns
asset_returns <- prices %>%
  group_by(symbol) %>%
  tq_transmute(adjusted,
               periodReturn,
               period = "monthly",
               type = "log",
               col_rename = "monthly.returns")
```

    ## Warning: `type_convert()` only converts columns of type 'character'.
    ## - `df` has no columns of type 'character'

    ## Warning: `type_convert()` only converts columns of type 'character'.
    ## - `df` has no columns of type 'character'

    ## Warning: `type_convert()` only converts columns of type 'character'.
    ## - `df` has no columns of type 'character'

    ## Warning: `type_convert()` only converts columns of type 'character'.
    ## - `df` has no columns of type 'character'

    ## Warning: `type_convert()` only converts columns of type 'character'.
    ## - `df` has no columns of type 'character'

``` r
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
```

    ## Warning: `type_convert()` only converts columns of type 'character'.
    ## - `df` has no columns of type 'character'

    ## Warning: Non-numeric columns being dropped: date

    ## Using column `date` for date_var.

``` r
## visualizing portfolio returns
```

``` r
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
```

<div id="htmlwidget-220aa14bdf9e29f54cd9" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-220aa14bdf9e29f54cd9">{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"Portfolio Monthly Returns"},"yAxis":{"title":{"text":null}},"credits":{"enabled":false},"exporting":{"enabled":true},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0},"treemap":{"layoutAlgorithm":"squarified"}},"series":[{"data":[[1454025600000,-0.0440886416786518],[1456704000000,-0.0105338788238279],[1459382400000,0.0676227894981065],[1461888000000,0.0165407361373977],[1464652800000,0.0186224292501149],[1467244800000,-0.000238113407500351],[1469750400000,0.0349419035373788],[1472601600000,0.00542286931221003],[1475193600000,0.00354691234496318],[1477872000000,-0.0192581532630111],[1480464000000,0.0496752437000252],[1483056000000,0.0205555731439953],[1485820800000,0.0112239253030442],[1488240000000,0.035836698921923],[1490918400000,-0.00366736733981521],[1493337600000,0.00345729015127882],[1496188800000,0.00537146210538619],[1498780800000,0.0142600108247095],[1501459200000,0.0223171029265503],[1504137600000,-0.00271111230145693],[1506643200000,0.0326157480005975],[1509408000000,0.0206187904614905],[1512000000000,0.026157137857056],[1514505600000,0.01531922947028],[1517356800000,0.056464349799028],[1519776000000,-0.0439842993489308],[1522281600000,-0.0250687829918851],[1525046400000,0.0166808678910217],[1527724800000,0.0226672403668771],[1530230400000,0.00256068598337955],[1532995200000,0.0369666793018677],[1535673600000,0.0252313788629066],[1538092800000,0.00697368944494858],[1540944000000,-0.0769639988266412],[1543536000000,0.0175624675850699],[1546214400000,-0.102343382094724],[1548892800000,0.0766442782662953],[1551312000000,0.0311307572400641],[1553817600000,0.0141538444278551],[1556582400000,0.0340278225416142],[1559260800000,-0.0721368163413223],[1561680000000,0.0724610880979855],[1564531200000,0.0098061641434648],[1567123200000,-0.0302384280149366],[1569801600000,0.0223422146534857],[1572480000000,0.0225100800037334],[1574985600000,0.0391675414756016],[1577750400000,0.0355668941478982],[1580428800000,-0.0198671913340306],[1582848000000,-0.0973686808484908],[1585612800000,-0.171353026927478],[1588204800000,0.138703764896459],[1590710400000,0.0408645066192483],[1593475200000,0.0109609409382954],[1596153600000,0.037420936453687],[1598832000000,0.0523632009832742],[1601424000000,-0.0557045236772213],[1604016000000,-0.0309385111254915],[1606694400000,0.129325082455842],[1609372800000,0.0439796926794587],[1611878400000,-0.000475984895828896],[1614297600000,0.0566739349105174],[1617148800000,0.0393350391141487],[1619740800000,0.0444330063684033],[1622160000000,0.0193159908253042],[1625011200000,0.023860981887772],[1627603200000,0.00885557502614076],[1630368000000,0.0249025598394874],[1632960000000,-0.0271163018238063],[1633392000000,0.0154768081670653]],"name":"Rebalanced Monthly","color":"cornflowerblue"}],"navigator":{"enabled":false},"scrollbar":{"enabled":false},"legend":{"enabled":true}},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#ECF0F1"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"stock","fonts":[],"debug":false},"evals":[],"jsHooks":[]}</script>

``` r
## portfolio returns distribution
```

``` r
hc_portfolio <-
  hist(portfolio_returns$returns, breaks = 50, plot = FALSE)

hchart(hc_portfolio,
       color = "cornflowerblue",
       name = "Portfolio") %>%
  hc_title(text = "Portfolio Returns Distribution") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE)
```

<div id="htmlwidget-d0a28f96fd133f4385e0" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-d0a28f96fd133f4385e0">{"x":{"hc_opts":{"chart":{"reflow":true,"zoomType":"x"},"title":{"text":"Portfolio Returns Distribution"},"yAxis":{"title":{"text":null}},"credits":{"enabled":false},"exporting":{"enabled":true},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0},"treemap":{"layoutAlgorithm":"squarified"}},"tooltip":{"formatter":"function() { return  this.point.name + '<br/>' + this.y; }"},"series":[{"data":[{"x":-0.1725,"y":1,"name":"(-0.175, -0.17]"},{"x":-0.1675,"y":0,"name":"(-0.17, -0.165]"},{"x":-0.1625,"y":0,"name":"(-0.165, -0.16]"},{"x":-0.1575,"y":0,"name":"(-0.16, -0.155]"},{"x":-0.1525,"y":0,"name":"(-0.155, -0.15]"},{"x":-0.1475,"y":0,"name":"(-0.15, -0.145]"},{"x":-0.1425,"y":0,"name":"(-0.145, -0.14]"},{"x":-0.1375,"y":0,"name":"(-0.14, -0.135]"},{"x":-0.1325,"y":0,"name":"(-0.135, -0.13]"},{"x":-0.1275,"y":0,"name":"(-0.13, -0.125]"},{"x":-0.1225,"y":0,"name":"(-0.125, -0.12]"},{"x":-0.1175,"y":0,"name":"(-0.12, -0.115]"},{"x":-0.1125,"y":0,"name":"(-0.115, -0.11]"},{"x":-0.1075,"y":0,"name":"(-0.11, -0.105]"},{"x":-0.1025,"y":1,"name":"(-0.105, -0.1]"},{"x":-0.0975,"y":1,"name":"(-0.1, -0.095]"},{"x":-0.0925,"y":0,"name":"(-0.095, -0.09]"},{"x":-0.0875,"y":0,"name":"(-0.09, -0.085]"},{"x":-0.0825,"y":0,"name":"(-0.085, -0.08]"},{"x":-0.0775,"y":1,"name":"(-0.08, -0.075]"},{"x":-0.0725,"y":1,"name":"(-0.075, -0.07]"},{"x":-0.0675,"y":0,"name":"(-0.07, -0.065]"},{"x":-0.0625,"y":0,"name":"(-0.065, -0.06]"},{"x":-0.0575,"y":1,"name":"(-0.06, -0.055]"},{"x":-0.0525,"y":0,"name":"(-0.055, -0.05]"},{"x":-0.0475,"y":0,"name":"(-0.05, -0.045]"},{"x":-0.0425,"y":2,"name":"(-0.045, -0.04]"},{"x":-0.0375,"y":0,"name":"(-0.04, -0.035]"},{"x":-0.0325,"y":2,"name":"(-0.035, -0.03]"},{"x":-0.0275,"y":2,"name":"(-0.03, -0.025]"},{"x":-0.0225,"y":0,"name":"(-0.025, -0.02]"},{"x":-0.0175,"y":2,"name":"(-0.02, -0.015]"},{"x":-0.0125,"y":1,"name":"(-0.015, -0.01]"},{"x":-0.00750000000000001,"y":0,"name":"(-0.01, -0.005]"},{"x":-0.0025,"y":4,"name":"(-0.005, 0]"},{"x":0.0025,"y":3,"name":"(0, 0.005]"},{"x":0.00749999999999999,"y":5,"name":"(0.00499999999999999, 0.01]"},{"x":0.0125,"y":4,"name":"(0.00999999999999998, 0.015]"},{"x":0.0175,"y":7,"name":"(0.015, 0.02]"},{"x":0.0225,"y":8,"name":"(0.02, 0.025]"},{"x":0.0275,"y":2,"name":"(0.025, 0.03]"},{"x":0.0325,"y":4,"name":"(0.03, 0.035]"},{"x":0.0375,"y":6,"name":"(0.035, 0.04]"},{"x":0.0425,"y":3,"name":"(0.04, 0.045]"},{"x":0.0475,"y":1,"name":"(0.045, 0.05]"},{"x":0.0525,"y":1,"name":"(0.05, 0.055]"},{"x":0.0575,"y":2,"name":"(0.055, 0.06]"},{"x":0.0625,"y":0,"name":"(0.06, 0.065]"},{"x":0.0675,"y":1,"name":"(0.065, 0.07]"},{"x":0.0725,"y":1,"name":"(0.07, 0.075]"},{"x":0.0775,"y":1,"name":"(0.075, 0.08]"},{"x":0.0825,"y":0,"name":"(0.08, 0.085]"},{"x":0.0875,"y":0,"name":"(0.085, 0.09]"},{"x":0.0925,"y":0,"name":"(0.09, 0.095]"},{"x":0.0975,"y":0,"name":"(0.095, 0.1]"},{"x":0.1025,"y":0,"name":"(0.1, 0.105]"},{"x":0.1075,"y":0,"name":"(0.105, 0.11]"},{"x":0.1125,"y":0,"name":"(0.11, 0.115]"},{"x":0.1175,"y":0,"name":"(0.115, 0.12]"},{"x":0.1225,"y":0,"name":"(0.12, 0.125]"},{"x":0.1275,"y":1,"name":"(0.125, 0.13]"},{"x":0.1325,"y":0,"name":"(0.13, 0.135]"},{"x":0.1375,"y":1,"name":"(0.135, 0.14]"}],"type":"column","pointRange":0.005,"groupPadding":0,"pointPadding":0,"borderWidth":0,"color":"cornflowerblue","name":"Portfolio"}]},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#ECF0F1"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":["hc_opts.tooltip.formatter"],"jsHooks":[]}</script>
