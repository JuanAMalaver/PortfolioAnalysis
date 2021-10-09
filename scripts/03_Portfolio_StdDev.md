Portfolio Standard Deviation
================
Juan Malaver
October 8, 2021

This script collects daily prices for 5 assets and calculates standard
deviation of returns.

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
               col_rename = "Ra")
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
               returns_col = Ra,
               weights = wts,
               col_rename = "Ra",
               rebalance_on = "months")
```

    ## Warning: `type_convert()` only converts columns of type 'character'.
    ## - `df` has no columns of type 'character'

``` r
## calculating standard deviation for underlying assets
asset_stdev <- asset_returns %>%
  tq_performance(Ra = Ra,
                 Rb = NULL,
                 performance_fun = table.Stats) %>%
  select(Stdev)
```

    ## Adding missing grouping variables: `symbol`

``` r
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

## visualizing portfolio returns
```

``` r
hchart(portfolio_groups, "scatter", hcaes(x = date, y = Ra, group = group)) %>%
  hc_title(text = "Portfolio Monthly Return Anomalies") %>%
  hc_xAxis(title = list(text = "")) %>%
  hc_yAxis(title = list(text = "")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE) %>%
  hc_tooltip(pointFormat = "x: {point.x:%b \'%y} <br> y: {point.y}")
```

<div id="htmlwidget-e2cb0e493462fe52628b" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-e2cb0e493462fe52628b">{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"Portfolio Monthly Return Anomalies"},"yAxis":{"title":{"text":""},"type":"linear"},"credits":{"enabled":false},"exporting":{"enabled":true},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":true},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}}},"series":[{"name":"Above","data":[{"date":"2016-03-31","Ra":0.0676227187879237,"x":1459382400000,"y":0.0676227187879237},{"date":"2019-01-31","Ra":0.0766442532241267,"x":1548892800000,"y":0.0766442532241267},{"date":"2019-06-28","Ra":0.0724610612529561,"x":1561680000000,"y":0.0724610612529561},{"date":"2020-04-30","Ra":0.13870380259661,"x":1588204800000,"y":0.13870380259661},{"date":"2020-11-30","Ra":0.129325061178629,"x":1606694400000,"y":0.129325061178629}],"type":"scatter"},{"name":"Below","data":[{"date":"2016-01-29","Ra":-0.0440886887708221,"x":1454025600000,"y":-0.0440886887708221},{"date":"2018-02-28","Ra":-0.043984247093432,"x":1519776000000,"y":-0.043984247093432},{"date":"2018-10-31","Ra":-0.0769641631795551,"x":1540944000000,"y":-0.0769641631795551},{"date":"2018-12-31","Ra":-0.102343538142006,"x":1546214400000,"y":-0.102343538142006},{"date":"2019-05-31","Ra":-0.0721367680168308,"x":1559260800000,"y":-0.0721367680168308},{"date":"2020-02-28","Ra":-0.097368650924342,"x":1582848000000,"y":-0.097368650924342},{"date":"2020-03-31","Ra":-0.171353101785379,"x":1585612800000,"y":-0.171353101785379},{"date":"2020-09-30","Ra":-0.0557044511789996,"x":1601424000000,"y":-0.0557044511789996}],"type":"scatter"},{"name":"Between","data":[{"date":"2016-02-29","Ra":-0.0105336573373762,"x":1456704000000,"y":-0.0105336573373762},{"date":"2016-04-29","Ra":0.0165407020934287,"x":1461888000000,"y":0.0165407020934287},{"date":"2016-05-31","Ra":0.0186222683064996,"x":1464652800000,"y":0.0186222683064996},{"date":"2016-06-30","Ra":-0.00023796925936681,"x":1467244800000,"y":-0.00023796925936681},{"date":"2016-07-29","Ra":0.0349418949630835,"x":1469750400000,"y":0.0349418949630835},{"date":"2016-08-31","Ra":0.00542288246805378,"x":1472601600000,"y":0.00542288246805378},{"date":"2016-09-30","Ra":0.00354686783157288,"x":1475193600000,"y":0.00354686783157288},{"date":"2016-10-31","Ra":-0.0192581608191674,"x":1477872000000,"y":-0.0192581608191674},{"date":"2016-11-30","Ra":0.0496752670633771,"x":1480464000000,"y":0.0496752670633771},{"date":"2016-12-30","Ra":0.0205555605920942,"x":1483056000000,"y":0.0205555605920942},{"date":"2017-01-31","Ra":0.0112238785386345,"x":1485820800000,"y":0.0112238785386345},{"date":"2017-02-28","Ra":0.0358366519242836,"x":1488240000000,"y":0.0358366519242836},{"date":"2017-03-31","Ra":-0.00366732419079963,"x":1490918400000,"y":-0.00366732419079963},{"date":"2017-04-28","Ra":0.00345728053075267,"x":1493337600000,"y":0.00345728053075267},{"date":"2017-05-31","Ra":0.0053714415355004,"x":1496188800000,"y":0.0053714415355004},{"date":"2017-06-30","Ra":0.0142600401912472,"x":1498780800000,"y":0.0142600401912472},{"date":"2017-07-31","Ra":0.02231715841208,"x":1501459200000,"y":0.02231715841208},{"date":"2017-08-31","Ra":-0.00271115931894084,"x":1504137600000,"y":-0.00271115931894084},{"date":"2017-09-29","Ra":0.0326157268373233,"x":1506643200000,"y":0.0326157268373233},{"date":"2017-10-31","Ra":0.0206188860564189,"x":1509408000000,"y":0.0206188860564189},{"date":"2017-11-30","Ra":0.0261571667752525,"x":1512000000000,"y":0.0261571667752525},{"date":"2017-12-29","Ra":0.0153191771583869,"x":1514505600000,"y":0.0153191771583869},{"date":"2018-01-31","Ra":0.0564642251037892,"x":1517356800000,"y":0.0564642251037892},{"date":"2018-03-29","Ra":-0.0250688541790306,"x":1522281600000,"y":-0.0250688541790306},{"date":"2018-04-30","Ra":0.0166809706836304,"x":1525046400000,"y":0.0166809706836304},{"date":"2018-05-31","Ra":0.0226671717271234,"x":1527724800000,"y":0.0226671717271234},{"date":"2018-06-29","Ra":0.00256071572799921,"x":1530230400000,"y":0.00256071572799921},{"date":"2018-07-31","Ra":0.0369668258191318,"x":1532995200000,"y":0.0369668258191318},{"date":"2018-08-31","Ra":0.0252312806593833,"x":1535673600000,"y":0.0252312806593833},{"date":"2018-09-28","Ra":0.00697375660334765,"x":1538092800000,"y":0.00697375660334765},{"date":"2018-11-30","Ra":0.0175626851846229,"x":1543536000000,"y":0.0175626851846229},{"date":"2019-02-28","Ra":0.0311307930590834,"x":1551312000000,"y":0.0311307930590834},{"date":"2019-03-29","Ra":0.0141538816182243,"x":1553817600000,"y":0.0141538816182243},{"date":"2019-04-30","Ra":0.0340277708848649,"x":1556582400000,"y":0.0340277708848649},{"date":"2019-07-31","Ra":0.00980613951694087,"x":1564531200000,"y":0.00980613951694087},{"date":"2019-08-30","Ra":-0.0302383734359145,"x":1567123200000,"y":-0.0302383734359145},{"date":"2019-09-30","Ra":0.0223422120344665,"x":1569801600000,"y":0.0223422120344665},{"date":"2019-10-31","Ra":0.0225100899696977,"x":1572480000000,"y":0.0225100899696977},{"date":"2019-11-29","Ra":0.0391675368808477,"x":1574985600000,"y":0.0391675368808477},{"date":"2019-12-31","Ra":0.0355668808460554,"x":1577750400000,"y":0.0355668808460554},{"date":"2020-01-31","Ra":-0.0198671305821397,"x":1580428800000,"y":-0.0198671305821397},{"date":"2020-05-29","Ra":0.0408644494746149,"x":1590710400000,"y":0.0408644494746149},{"date":"2020-06-30","Ra":0.0109609473393757,"x":1593475200000,"y":0.0109609473393757},{"date":"2020-07-31","Ra":0.0374209256763876,"x":1596153600000,"y":0.0374209256763876},{"date":"2020-08-31","Ra":0.0523631604687931,"x":1598832000000,"y":0.0523631604687931},{"date":"2020-10-30","Ra":-0.0309385299327278,"x":1604016000000,"y":-0.0309385299327278},{"date":"2020-12-31","Ra":0.0439797163492375,"x":1609372800000,"y":0.0439797163492375},{"date":"2021-01-29","Ra":-0.000475984895829007,"x":1611878400000,"y":-0.000475984895829007},{"date":"2021-02-26","Ra":0.0566739629601629,"x":1614297600000,"y":0.0566739629601629},{"date":"2021-03-31","Ra":0.0393349929286047,"x":1617148800000,"y":0.0393349929286047},{"date":"2021-04-30","Ra":0.0444330250250842,"x":1619740800000,"y":0.0444330250250842},{"date":"2021-05-28","Ra":0.0193159903045217,"x":1622160000000,"y":0.0193159903045217},{"date":"2021-06-30","Ra":0.0238609818877722,"x":1625011200000,"y":0.0238609818877722},{"date":"2021-07-30","Ra":0.00885557502614098,"x":1627603200000,"y":0.00885557502614098},{"date":"2021-08-31","Ra":0.0249025598394874,"x":1630368000000,"y":0.0249025598394874},{"date":"2021-09-30","Ra":-0.0271163018238065,"x":1632960000000,"y":-0.0271163018238065},{"date":"2021-10-07","Ra":0.0252308546507629,"x":1633564800000,"y":0.0252308546507629}],"type":"scatter"}],"xAxis":{"type":"datetime","title":{"text":""}},"navigator":{"enabled":false},"scrollbar":{"enabled":false},"legend":{"enabled":false},"tooltip":{"pointFormat":"x: {point.x:%b '%y} <br> y: {point.y}"}},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#ECF0F1"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":[],"jsHooks":[]}</script>
