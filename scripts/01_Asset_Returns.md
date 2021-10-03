Daily Prices to Monthly Returns
================
Juan Malaver
October 3, 2021

This script collects daily price data for 5 assets and coverts to
monthly returns.

``` r
## loading libraries
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.1.1

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.1.1

    ## Warning: package 'tibble' was built under R version 4.1.1

    ## Warning: package 'tidyr' was built under R version 4.1.1

    ## Warning: package 'readr' was built under R version 4.1.1

    ## Warning: package 'purrr' was built under R version 4.1.1

    ## Warning: package 'dplyr' was built under R version 4.1.1

    ## Warning: package 'stringr' was built under R version 4.1.1

    ## Warning: package 'forcats' was built under R version 4.1.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(tidyquant)
```

    ## Warning: package 'tidyquant' was built under R version 4.1.1

    ## Loading required package: lubridate

    ## Warning: package 'lubridate' was built under R version 4.1.1

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

    ## Loading required package: PerformanceAnalytics

    ## Warning: package 'PerformanceAnalytics' was built under R version 4.1.1

    ## Loading required package: xts

    ## Warning: package 'xts' was built under R version 4.1.1

    ## Loading required package: zoo

    ## Warning: package 'zoo' was built under R version 4.1.1

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

    ## Warning: package 'quantmod' was built under R version 4.1.1

    ## Loading required package: TTR

    ## Warning: package 'TTR' was built under R version 4.1.1

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## == Need to Learn tidyquant? ====================================================
    ## Business Science offers a 1-hour course - Learning Lab #9: Performance Analysis & Portfolio Optimization with tidyquant!
    ## </> Learn more at: https://university.business-science.io/p/learning-labs-pro </>

``` r
library(timetk)
```

    ## Warning: package 'timetk' was built under R version 4.1.1

``` r
library(highcharter)
```

    ## Warning: package 'highcharter' was built under R version 4.1.1

    ## Highcharts (www.highcharts.com) is a Highsoft software product which is

    ## not free for commercial and Governmental use

``` r
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
               col_rename = "monthly.returns") %>%
  pivot_wider(names_from = symbol, values_from = monthly.returns) %>%
  tk_xts()
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

    ## Warning: Non-numeric columns being dropped: date

    ## Using column `date` for date_var.

``` r
## visualizing asset returns
```

``` r
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
```

<div id="htmlwidget-f1670ab5f8d7c6334d16" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-f1670ab5f8d7c6334d16">{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"Monthly Log Returns"},"yAxis":{"title":{"text":null}},"credits":{"enabled":false},"exporting":{"enabled":true},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0},"treemap":{"layoutAlgorithm":"squarified"}},"series":[{"data":[[1454025600000,-0.03699049763266],[1456704000000,-0.000826254141486394],[1459382400000,0.0651003800585272],[1461888000000,0.00393344602530312],[1464652800000,0.0168685289629413],[1467244800000,0.00346992324411548],[1469750400000,0.0358218215971838],[1472601600000,0.00119663259443501],[1475193600000,5.80988418534521e-05],[1477872000000,-0.0174890950809629],[1480464000000,0.0361761240114885],[1483056000000,0.0200689016255674],[1485820800000,0.0177365930594193],[1488240000000,0.0385392814716729],[1490918400000,0.00124922644778744],[1493337600000,0.0098771121677251],[1496188800000,0.0140142762534609],[1498780800000,0.00635464725074458],[1501459200000,0.0203460100381664],[1504137600000,0.0029133998979207],[1506643200000,0.0199489714413721],[1509408000000,0.0232908929929474],[1512000000000,0.0301079406813587],[1514505600000,0.0120549041810987],[1517356800000,0.0548283456337275],[1519776000000,-0.0370379797587697],[1522281600000,-0.0277931471979013],[1525046400000,0.00515489642881645],[1527724800000,0.0240182509198063],[1530230400000,0.005734565459217],[1532995200000,0.0363767465711694],[1535673600000,0.0314208610064987],[1538092800000,0.00592798362751449],[1540944000000,-0.0716080995148424],[1543536000000,0.0183795596348277],[1546214400000,-0.0921685246298688],[1548892800000,0.0770218194494886],[1551312000000,0.0319013831495385],[1553817600000,0.017938793250778],[1556582400000,0.0400399518230861],[1559260800000,-0.0658953980519728],[1561680000000,0.067272173579028],[1564531200000,0.0150062251203123],[1567123200000,-0.0168850405156693],[1569801600000,0.0192709711977103],[1572480000000,0.0218639188437471],[1574985600000,0.0355583415786652],[1577750400000,0.028641350879117],[1580428800000,-0.000403959959976044],[1582848000000,-0.0824751639664085],[1585612800000,-0.133384284291427],[1588204800000,0.119544617103017],[1590710400000,0.0465451670021835],[1593475200000,0.0175788941035425],[1596153600000,0.0572233432844488],[1598832000000,0.0674685633545184],[1601424000000,-0.0381627034685494],[1604016000000,-0.0252496614903119],[1606694400000,0.103257453372282],[1609372800000,0.0363786094561552],[1611878400000,-0.0102426620180894],[1614297600000,0.0274258903815747],[1617148800000,0.0443989349088403],[1619740800000,0.0515581876695903],[1622160000000,0.00654464944451463],[1625011200000,0.0221796270676683],[1627603200000,0.0241192376158545],[1630368000000,0.0293256494477845],[1632960000000,-0.0477264874306327],[1633046400000,0.0118141126472016]],"name":"SPY"},{"data":[[1454025600000,-0.0732194547443738],[1456704000000,-0.0294345400010298],[1459382400000,0.0691448964663669],[1461888000000,0.0353671899655082],[1464652800000,0.0186999158108017],[1467244800000,-0.0328609927414439],[1469750400000,0.0339746005509771],[1472601600000,0.0377559620126919],[1475193600000,-0.0284181212800427],[1477872000000,0.0225421798818608],[1480464000000,0.13131244495495],[1483056000000,0.0368764810913737],[1485820800000,0.00257724358501058],[1488240000000,0.0514220035671825],[1490918400000,-0.0300068472774335],[1493337600000,-0.00846391467436071],[1496188800000,-0.0119710154709996],[1498780800000,0.0630999688802364],[1501459200000,0.0168814740680755],[1504137600000,-0.0156662964432694],[1506643200000,0.0500562240223253],[1509408000000,0.0282140400509697],[1512000000000,0.0340015783193439],[1514505600000,0.0187053302375368],[1517356800000,0.0635078425155877],[1519776000000,-0.0296899920253146],[1522281600000,-0.0424411852784885],[1525046400000,-0.00436200707106343],[1527724800000,-0.00988472430385361],[1530230400000,-0.01762369891133],[1532995200000,0.0498819686908356],[1535673600000,0.0135041025060557],[1538092800000,-0.0224038953992852],[1540944000000,-0.0482826145618335],[1543536000000,0.0259168128299563],[1546214400000,-0.117983580270186],[1548892800000,0.0852604994607538],[1551312000000,0.0221130150083207],[1553817600000,-0.0259272235640753],[1556582400000,0.086038617300621],[1559260800000,-0.0744374590596968],[1561680000000,0.0644169595978974],[1564531200000,0.0232775799271614],[1567123200000,-0.0482238610409336],[1569801600000,0.0444201726709821],[1572480000000,0.0246925534581059],[1574985600000,0.0492877979937494],[1577750400000,0.0257580424418721],[1580428800000,-0.0270020800730337],[1582848000000,-0.119327865808293],[1585612800000,-0.236177363778592],[1588204800000,0.0904078453671455],[1590710400000,0.0268413966464411],[1593475200000,-0.00524182230803897],[1596153600000,0.0377404394125327],[1598832000000,0.0419697080289335],[1601424000000,-0.0348214943926651],[1604016000000,-0.00876285764136262],[1606694400000,0.155706118741961],[1609372800000,0.0611519703539068],[1611878400000,-0.0181418383934845],[1614297600000,0.109806684276471],[1617148800000,0.0568755545812146],[1619740800000,0.0628851979582126],[1622160000000,0.0466078146408767],[1625011200000,-0.0308654628537436],[1627603200000,-0.00464412309656888],[1630368000000,0.050197382410556],[1632960000000,-0.0185967997902141],[1633046400000,0.0163852236535256]],"name":"XLF"},{"data":[[1454025600000,-0.0352751562299244],[1456704000000,-0.0284013524556132],[1459382400000,0.0968172829265231],[1461888000000,0.086769009171964],[1464652800000,-0.00937721631399968],[1467244800000,0.0268435965580263],[1469750400000,-0.0126827312047181],[1472601600000,0.0166315639008525],[1475193600000,0.0362181382153955],[1477872000000,-0.0285877432133267],[1480464000000,0.0812751450329269],[1483056000000,0.0171591565879086],[1485820800000,-0.0326567787209581],[1488240000000,-0.0210711560588378],[1490918400000,-0.0149714650347455],[1493337600000,-0.0299135226332583],[1496188800000,-0.0360182134065],[1498780800000,-0.00111742233277458],[1501459200000,0.0258492984104116],[1504137600000,-0.0563464030001478],[1506643200000,0.0969660500443764],[1509408000000,-0.00835821659088159],[1512000000000,0.0173713349088008],[1514505600000,0.0513526951606483],[1517356800000,0.0352152476665716],[1519776000000,-0.114681464515775],[1522281600000,0.0170835015545281],[1525046400000,0.0907006805051313],[1527724800000,0.0295023360781524],[1530230400000,0.00567914752015977],[1532995200000,0.0154189831342065],[1535673600000,-0.0353690945655562],[1538092800000,0.0241652939157492],[1540944000000,-0.120228307620264],[1543536000000,-0.0157578512438734],[1546214400000,-0.132836720098945],[1548892800000,0.106266912564224],[1551312000000,0.0227862276768036],[1553817600000,0.0210577842315389],[1556582400000,-0.000151265279264377],[1559260800000,-0.117688609135524],[1561680000000,0.0897965795992315],[1564531200000,-0.0159800880233303],[1567123200000,-0.0869243789757305],[1569801600000,0.0385887350943164],[1572480000000,-0.0211684945342282],[1574985600000,0.0159181788290258],[1577750400000,0.0585161531243301],[1580428800000,-0.116451534092238],[1582848000000,-0.165914789610663],[1585612800000,-0.421151839239702],[1588204800000,0.268223486745443],[1590710400000,0.0198026106082654],[1593475200000,-0.011015930009387],[1596153600000,-0.0492790763841686],[1598832000000,-0.0106026008479335],[1601424000000,-0.157901776645327],[1604016000000,-0.0419356481661746],[1606694400000,0.246816548149747],[1609372800000,0.043826734121329],[1611878400000,0.0367821218929748],[1614297600000,0.202587906867531],[1617148800000,0.0292770661063083],[1619740800000,0.00670390304391327],[1622160000000,0.0555260757447144],[1625011200000,0.0414676663770576],[1627603200000,-0.0868257324979146],[1630368000000,-0.0202481229179065],[1632960000000,0.0855489797377158],[1633046400000,0.0330436935793721]],"name":"XLE"},{"data":[[1454025600000,-0.0246687949808532],[1456704000000,-0.00656862372680673],[1459382400000,0.0845406207219249],[1461888000000,-0.0515781911549404],[1464652800000,0.0477382624834065],[1467244800000,-0.0138736667277527],[1469750400000,0.0686085695404082],[1472601600000,0.0115584525548221],[1475193600000,0.0208002702081994],[1477872000000,-0.00756325717950539],[1480464000000,0.00168565921615714],[1483056000000,0.0223470565891838],[1485820800000,0.0349488471155419],[1488240000000,0.0443300237903121],[1490918400000,0.0220162974574641],[1493337600000,0.019872440752858],[1496188800000,0.0387749240089535],[1498780800000,-0.0285211274810951],[1501459200000,0.0436249528594237],[1504137600000,0.0287975639371169],[1506643200000,0.00814531565828286],[1509408000000,0.0631097585714149],[1512000000000,0.0140393359384934],[1514505600000,0.00544308645052023],[1517356800000,0.0680018477235116],[1519776000000,-0.00409884443922907],[1522281600000,-0.0380527111206926],[1525046400000,0.000611135625413164],[1527724800000,0.06562629364829],[1530230400000,-0.00263498394656567],[1532995200000,0.0206576061852653],[1535673600000,0.0639036337892808],[1538092800000,-0.000192478744764033],[1540944000000,-0.0834337182207441],[1543536000000,-0.0198196761501842],[1546214400000,-0.0874824501830851],[1548892800000,0.0670763728401421],[1551312000000,0.0668180069405629],[1553817600000,0.046584568356832],[1556582400000,0.0617051024453768],[1559260800000,-0.0906329114366904],[1561680000000,0.0856113006703033],[1564531200000,0.0343841049133076],[1567123200000,-0.0154714394469286],[1569801600000,0.0156061846633411],[1572480000000,0.038250759974485],[1574985600000,0.0522728608645842],[1577750400000,0.0422988633511166],[1580428800000,0.0391494565451986],[1582848000000,-0.075812084834396],[1585612800000,-0.0896834575381536],[1588204800000,0.128713954876613],[1590710400000,0.0693063870454356],[1593475200000,0.0671823106841624],[1596153600000,0.0552905532232739],[1598832000000,0.112264005587782],[1601424000000,-0.0548268032121011],[1604016000000,-0.0513384531177214],[1606694400000,0.107811101450737],[1609372800000,0.0539353233894649],[1611878400000,-0.00841873098699635],[1614297600000,0.0135584797427646],[1617148800000,0.0182002760013018],[1619740800000,0.0505776758550852],[1622160000000,-0.00934937621026395],[1625011200000,0.0665959537609068],[1627603200000,0.038136438792108],[1630368000000,0.034974492748537],[1632960000000,-0.0601742442789461],[1633046400000,0.0149559097012784]],"name":"XLK"},{"data":[[1454025600000,-0.0621191145375431],[1456704000000,-0.00361705135810285],[1459382400000,0.026714568372874],[1461888000000,0.0292239052725264],[1464652800000,0.0221062021121312],[1467244800000,0.00905064155232142],[1469750400000,0.0475210258997314],[1472601600000,-0.0329849509165166],[1475193600000,-0.00510920293057137],[1477872000000,-0.0681416387917796],[1480464000000,0.0204255158945427],[1483056000000,0.0071372535456872],[1485820800000,0.0226592653868594],[1488240000000,0.0614589939749003],[1490918400000,-0.00481784164666579],[1493337600000,0.0152146754359535],[1496188800000,0.00765279362342013],[1498780800000,0.0446587100970582],[1501459200000,0.00816945571657102],[1504137600000,0.0173724557051292],[1506643200000,0.00907229470412115],[1509408000000,-0.00761461098500978],[1512000000000,0.0286809416339781],[1514505600000,-0.00551987099814988],[1517356800000,0.0634948995844365],[1519776000000,-0.0459905905274422],[1522281600000,-0.0295998101319741],[1525046400000,0.0105096404023605],[1527724800000,0.0018218664251153],[1530230400000,0.016358780634048],[1532995200000,0.0634821143565484],[1535673600000,0.0423812993700447],[1538092800000,0.0291143372729924],[1540944000000,-0.0701947217090701],[1543536000000,0.0777332671903451],[1546214400000,-0.0982045161724765],[1548892800000,0.0469666621850739],[1551312000000,0.0107505857189995],[1553817600000,0.00480754339701411],[1556582400000,-0.0275140454691583],[1559260800000,-0.0224322187229937],[1561680000000,0.0638572411397797],[1564531200000,-0.0163241841013774],[1567123200000,-0.00594266821393989],[1569801600000,-0.00105620533669972],[1572480000000,0.0499886215059083],[1574985600000,0.0488154238668652],[1577750400000,0.0341625724792948],[1580428800000,-0.0270663790046756],[1582848000000,-0.0681358077344409],[1585612800000,-0.0396493799368372],[1588204800000,0.118560497474831],[1590710400000,0.0323594331123494],[1593475200000,-0.024728908489662],[1596153600000,0.0531253732527528],[1598832000000,0.0255405958245008],[1601424000000,-0.0220459873244359],[1604016000000,-0.0368874656859186],[1606694400000,0.0764799195838929],[1609372800000,0.0372749114209602],[1611878400000,0.0139188711703954],[1614297600000,-0.0212624831429587],[1617148800000,0.0394834749573684],[1619740800000,0.0385648763774371],[1622160000000,0.0185364463268796],[1625011200000,0.0227291850373945],[1627603200000,0.0480526166677029],[1630368000000,0.0228915814946379],[1632960000000,-0.0602826480122447],[1633046400000,0.000235628162822211]],"name":"XLV"}],"navigator":{"enabled":false},"scrollbar":{"enabled":false},"legend":{"enabled":true}},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#ECF0F1"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"stock","fonts":[],"debug":false},"evals":[],"jsHooks":[]}</script>

``` r
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
```

``` r
hc_hist_fun(1, asset_returns, "cornflowerblue")
```

<div id="htmlwidget-0b61ff676b24d9abc603" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-0b61ff676b24d9abc603">{"x":{"hc_opts":{"chart":{"reflow":true,"zoomType":"x"},"title":{"text":"SPY Log Returns Distribution"},"yAxis":{"title":{"text":null}},"credits":{"enabled":false},"exporting":{"enabled":true},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0},"treemap":{"layoutAlgorithm":"squarified"}},"tooltip":{"formatter":"function() { return  this.point.name + '<br/>' + this.y; }"},"series":[{"data":[{"x":-0.1325,"y":1,"name":"(-0.135, -0.13]"},{"x":-0.1275,"y":0,"name":"(-0.13, -0.125]"},{"x":-0.1225,"y":0,"name":"(-0.125, -0.12]"},{"x":-0.1175,"y":0,"name":"(-0.12, -0.115]"},{"x":-0.1125,"y":0,"name":"(-0.115, -0.11]"},{"x":-0.1075,"y":0,"name":"(-0.11, -0.105]"},{"x":-0.1025,"y":0,"name":"(-0.105, -0.1]"},{"x":-0.0975,"y":0,"name":"(-0.1, -0.095]"},{"x":-0.0925,"y":1,"name":"(-0.095, -0.09]"},{"x":-0.0875,"y":0,"name":"(-0.09, -0.085]"},{"x":-0.0825,"y":1,"name":"(-0.085, -0.08]"},{"x":-0.0775,"y":0,"name":"(-0.08, -0.075]"},{"x":-0.0725,"y":1,"name":"(-0.075, -0.07]"},{"x":-0.0675,"y":1,"name":"(-0.07, -0.065]"},{"x":-0.0625,"y":0,"name":"(-0.065, -0.06]"},{"x":-0.0575,"y":0,"name":"(-0.06, -0.055]"},{"x":-0.0525,"y":0,"name":"(-0.055, -0.05]"},{"x":-0.0475,"y":1,"name":"(-0.05, -0.045]"},{"x":-0.0425,"y":0,"name":"(-0.045, -0.04]"},{"x":-0.0375,"y":3,"name":"(-0.04, -0.035]"},{"x":-0.0325,"y":0,"name":"(-0.035, -0.03]"},{"x":-0.0275,"y":2,"name":"(-0.03, -0.025]"},{"x":-0.0225,"y":0,"name":"(-0.025, -0.02]"},{"x":-0.0175,"y":2,"name":"(-0.02, -0.015]"},{"x":-0.0125,"y":1,"name":"(-0.015, -0.01]"},{"x":-0.00750000000000001,"y":0,"name":"(-0.01, -0.005]"},{"x":-0.0025,"y":2,"name":"(-0.005, 0]"},{"x":0.00249999999999999,"y":6,"name":"(-1.38777878078145e-17, 0.00499999999999999]"},{"x":0.00749999999999998,"y":6,"name":"(0.00499999999999998, 0.00999999999999998]"},{"x":0.0125,"y":3,"name":"(0.00999999999999998, 0.015]"},{"x":0.0175,"y":8,"name":"(0.015, 0.02]"},{"x":0.0225,"y":7,"name":"(0.02, 0.025]"},{"x":0.0275,"y":3,"name":"(0.025, 0.03]"},{"x":0.0325,"y":3,"name":"(0.03, 0.035]"},{"x":0.0375,"y":6,"name":"(0.035, 0.04]"},{"x":0.0425,"y":2,"name":"(0.04, 0.045]"},{"x":0.0475,"y":1,"name":"(0.045, 0.05]"},{"x":0.0525,"y":2,"name":"(0.05, 0.055]"},{"x":0.0575,"y":1,"name":"(0.055, 0.06]"},{"x":0.0625,"y":0,"name":"(0.06, 0.065]"},{"x":0.0675,"y":3,"name":"(0.065, 0.07]"},{"x":0.0725,"y":0,"name":"(0.07, 0.075]"},{"x":0.0775,"y":1,"name":"(0.075, 0.08]"},{"x":0.0825,"y":0,"name":"(0.08, 0.085]"},{"x":0.0875,"y":0,"name":"(0.085, 0.09]"},{"x":0.0925,"y":0,"name":"(0.09, 0.095]"},{"x":0.0975,"y":0,"name":"(0.095, 0.1]"},{"x":0.1025,"y":1,"name":"(0.1, 0.105]"},{"x":0.1075,"y":0,"name":"(0.105, 0.11]"},{"x":0.1125,"y":0,"name":"(0.11, 0.115]"},{"x":0.1175,"y":1,"name":"(0.115, 0.12]"}],"type":"column","pointRange":0.005,"groupPadding":0,"pointPadding":0,"borderWidth":0,"color":"cornflowerblue"}],"legend":{"enabled":false}},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#ECF0F1"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":["hc_opts.tooltip.formatter"],"jsHooks":[]}</script>

``` r
hc_hist_fun(2, asset_returns, "green")
```

<div id="htmlwidget-1e3e84903cba52482dae" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-1e3e84903cba52482dae">{"x":{"hc_opts":{"chart":{"reflow":true,"zoomType":"x"},"title":{"text":"XLF Log Returns Distribution"},"yAxis":{"title":{"text":null}},"credits":{"enabled":false},"exporting":{"enabled":true},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0},"treemap":{"layoutAlgorithm":"squarified"}},"tooltip":{"formatter":"function() { return  this.point.name + '<br/>' + this.y; }"},"series":[{"data":[{"x":-0.235,"y":1,"name":"(-0.24, -0.23]"},{"x":-0.225,"y":0,"name":"(-0.23, -0.22]"},{"x":-0.215,"y":0,"name":"(-0.22, -0.21]"},{"x":-0.205,"y":0,"name":"(-0.21, -0.2]"},{"x":-0.195,"y":0,"name":"(-0.2, -0.19]"},{"x":-0.185,"y":0,"name":"(-0.19, -0.18]"},{"x":-0.175,"y":0,"name":"(-0.18, -0.17]"},{"x":-0.165,"y":0,"name":"(-0.17, -0.16]"},{"x":-0.155,"y":0,"name":"(-0.16, -0.15]"},{"x":-0.145,"y":0,"name":"(-0.15, -0.14]"},{"x":-0.135,"y":0,"name":"(-0.14, -0.13]"},{"x":-0.125,"y":0,"name":"(-0.13, -0.12]"},{"x":-0.115,"y":2,"name":"(-0.12, -0.11]"},{"x":-0.105,"y":0,"name":"(-0.11, -0.1]"},{"x":-0.095,"y":0,"name":"(-0.1, -0.09]"},{"x":-0.085,"y":0,"name":"(-0.09, -0.08]"},{"x":-0.075,"y":2,"name":"(-0.08, -0.07]"},{"x":-0.065,"y":0,"name":"(-0.07, -0.06]"},{"x":-0.055,"y":0,"name":"(-0.06, -0.05]"},{"x":-0.045,"y":3,"name":"(-0.05, -0.04]"},{"x":-0.035,"y":4,"name":"(-0.04, -0.03]"},{"x":-0.025,"y":6,"name":"(-0.03, -0.02]"},{"x":-0.015,"y":5,"name":"(-0.02, -0.01]"},{"x":-0.005,"y":6,"name":"(-0.01, 0]"},{"x":0.005,"y":1,"name":"(0, 0.01]"},{"x":0.015,"y":5,"name":"(0.01, 0.02]"},{"x":0.025,"y":8,"name":"(0.02, 0.03]"},{"x":0.035,"y":6,"name":"(0.03, 0.04]"},{"x":0.045,"y":5,"name":"(0.04, 0.05]"},{"x":0.055,"y":4,"name":"(0.05, 0.06]"},{"x":0.065,"y":6,"name":"(0.06, 0.07]"},{"x":0.075,"y":0,"name":"(0.07, 0.08]"},{"x":0.085,"y":2,"name":"(0.08, 0.09]"},{"x":0.095,"y":1,"name":"(0.09, 0.1]"},{"x":0.105,"y":1,"name":"(0.1, 0.11]"},{"x":0.115,"y":0,"name":"(0.11, 0.12]"},{"x":0.125,"y":0,"name":"(0.12, 0.13]"},{"x":0.135,"y":1,"name":"(0.13, 0.14]"},{"x":0.145,"y":0,"name":"(0.14, 0.15]"},{"x":0.155,"y":1,"name":"(0.15, 0.16]"}],"type":"column","pointRange":0.01,"groupPadding":0,"pointPadding":0,"borderWidth":0,"color":"green"}],"legend":{"enabled":false}},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#ECF0F1"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":["hc_opts.tooltip.formatter"],"jsHooks":[]}</script>

``` r
hc_hist_fun(3, asset_returns, "pink")
```

<div id="htmlwidget-48518693629b885066e3" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-48518693629b885066e3">{"x":{"hc_opts":{"chart":{"reflow":true,"zoomType":"x"},"title":{"text":"XLE Log Returns Distribution"},"yAxis":{"title":{"text":null}},"credits":{"enabled":false},"exporting":{"enabled":true},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0},"treemap":{"layoutAlgorithm":"squarified"}},"tooltip":{"formatter":"function() { return  this.point.name + '<br/>' + this.y; }"},"series":[{"data":[{"x":-0.425,"y":1,"name":"(-0.43, -0.42]"},{"x":-0.415,"y":0,"name":"(-0.42, -0.41]"},{"x":-0.405,"y":0,"name":"(-0.41, -0.4]"},{"x":-0.395,"y":0,"name":"(-0.4, -0.39]"},{"x":-0.385,"y":0,"name":"(-0.39, -0.38]"},{"x":-0.375,"y":0,"name":"(-0.38, -0.37]"},{"x":-0.365,"y":0,"name":"(-0.37, -0.36]"},{"x":-0.355,"y":0,"name":"(-0.36, -0.35]"},{"x":-0.345,"y":0,"name":"(-0.35, -0.34]"},{"x":-0.335,"y":0,"name":"(-0.34, -0.33]"},{"x":-0.325,"y":0,"name":"(-0.33, -0.32]"},{"x":-0.315,"y":0,"name":"(-0.32, -0.31]"},{"x":-0.305,"y":0,"name":"(-0.31, -0.3]"},{"x":-0.295,"y":0,"name":"(-0.3, -0.29]"},{"x":-0.285,"y":0,"name":"(-0.29, -0.28]"},{"x":-0.275,"y":0,"name":"(-0.28, -0.27]"},{"x":-0.265,"y":0,"name":"(-0.27, -0.26]"},{"x":-0.255,"y":0,"name":"(-0.26, -0.25]"},{"x":-0.245,"y":0,"name":"(-0.25, -0.24]"},{"x":-0.235,"y":0,"name":"(-0.24, -0.23]"},{"x":-0.225,"y":0,"name":"(-0.23, -0.22]"},{"x":-0.215,"y":0,"name":"(-0.22, -0.21]"},{"x":-0.205,"y":0,"name":"(-0.21, -0.2]"},{"x":-0.195,"y":0,"name":"(-0.2, -0.19]"},{"x":-0.185,"y":0,"name":"(-0.19, -0.18]"},{"x":-0.175,"y":0,"name":"(-0.18, -0.17]"},{"x":-0.165,"y":1,"name":"(-0.17, -0.16]"},{"x":-0.155,"y":1,"name":"(-0.16, -0.15]"},{"x":-0.145,"y":0,"name":"(-0.15, -0.14]"},{"x":-0.135,"y":1,"name":"(-0.14, -0.13]"},{"x":-0.125,"y":1,"name":"(-0.13, -0.12]"},{"x":-0.115,"y":3,"name":"(-0.12, -0.11]"},{"x":-0.105,"y":0,"name":"(-0.11, -0.1]"},{"x":-0.095,"y":0,"name":"(-0.1, -0.09]"},{"x":-0.085,"y":2,"name":"(-0.09, -0.08]"},{"x":-0.075,"y":0,"name":"(-0.08, -0.07]"},{"x":-0.065,"y":0,"name":"(-0.07, -0.06]"},{"x":-0.055,"y":1,"name":"(-0.0600000000000001, -0.05]"},{"x":-0.045,"y":2,"name":"(-0.05, -0.04]"},{"x":-0.035,"y":4,"name":"(-0.04, -0.03]"},{"x":-0.025,"y":6,"name":"(-0.03, -0.02]"},{"x":-0.015,"y":6,"name":"(-0.02, -0.01]"},{"x":-0.005,"y":4,"name":"(-0.01, 0]"},{"x":0.00499999999999998,"y":2,"name":"(-2.77555756156289e-17, 0.00999999999999998]"},{"x":0.015,"y":7,"name":"(0.00999999999999995, 0.02]"},{"x":0.025,"y":7,"name":"(0.02, 0.03]"},{"x":0.035,"y":5,"name":"(0.03, 0.04]"},{"x":0.045,"y":2,"name":"(0.04, 0.05]"},{"x":0.055,"y":3,"name":"(0.05, 0.06]"},{"x":0.065,"y":0,"name":"(0.06, 0.07]"},{"x":0.075,"y":0,"name":"(0.07, 0.08]"},{"x":0.085,"y":4,"name":"(0.08, 0.09]"},{"x":0.095,"y":3,"name":"(0.09, 0.1]"},{"x":0.105,"y":1,"name":"(0.1, 0.11]"},{"x":0.115,"y":0,"name":"(0.11, 0.12]"},{"x":0.125,"y":0,"name":"(0.12, 0.13]"},{"x":0.135,"y":0,"name":"(0.13, 0.14]"},{"x":0.145,"y":0,"name":"(0.14, 0.15]"},{"x":0.155,"y":0,"name":"(0.15, 0.16]"},{"x":0.165,"y":0,"name":"(0.16, 0.17]"},{"x":0.175,"y":0,"name":"(0.17, 0.18]"},{"x":0.185,"y":0,"name":"(0.18, 0.19]"},{"x":0.195,"y":0,"name":"(0.19, 0.2]"},{"x":0.205,"y":1,"name":"(0.2, 0.21]"},{"x":0.215,"y":0,"name":"(0.21, 0.22]"},{"x":0.225,"y":0,"name":"(0.22, 0.23]"},{"x":0.235,"y":0,"name":"(0.23, 0.24]"},{"x":0.245,"y":1,"name":"(0.24, 0.25]"},{"x":0.255,"y":0,"name":"(0.25, 0.26]"},{"x":0.265,"y":1,"name":"(0.26, 0.27]"}],"type":"column","pointRange":0.01,"groupPadding":0,"pointPadding":0,"borderWidth":0,"color":"pink"}],"legend":{"enabled":false}},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#ECF0F1"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":["hc_opts.tooltip.formatter"],"jsHooks":[]}</script>

``` r
hc_hist_fun(4, asset_returns, "purple")
```

<div id="htmlwidget-51a61358ac334a8bba07" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-51a61358ac334a8bba07">{"x":{"hc_opts":{"chart":{"reflow":true,"zoomType":"x"},"title":{"text":"XLK Log Returns Distribution"},"yAxis":{"title":{"text":null}},"credits":{"enabled":false},"exporting":{"enabled":true},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0},"treemap":{"layoutAlgorithm":"squarified"}},"tooltip":{"formatter":"function() { return  this.point.name + '<br/>' + this.y; }"},"series":[{"data":[{"x":-0.0925,"y":1,"name":"(-0.095, -0.09]"},{"x":-0.0875,"y":2,"name":"(-0.09, -0.085]"},{"x":-0.0825,"y":1,"name":"(-0.085, -0.08]"},{"x":-0.0775,"y":1,"name":"(-0.08, -0.075]"},{"x":-0.0725,"y":0,"name":"(-0.075, -0.07]"},{"x":-0.0675,"y":0,"name":"(-0.07, -0.065]"},{"x":-0.0625,"y":1,"name":"(-0.065, -0.06]"},{"x":-0.0575,"y":0,"name":"(-0.06, -0.055]"},{"x":-0.0525,"y":3,"name":"(-0.055, -0.05]"},{"x":-0.0475,"y":0,"name":"(-0.05, -0.045]"},{"x":-0.0425,"y":0,"name":"(-0.045, -0.04]"},{"x":-0.0375,"y":1,"name":"(-0.04, -0.035]"},{"x":-0.0325,"y":0,"name":"(-0.035, -0.03]"},{"x":-0.0275,"y":1,"name":"(-0.03, -0.025]"},{"x":-0.0225,"y":1,"name":"(-0.025, -0.02]"},{"x":-0.0175,"y":2,"name":"(-0.02, -0.015]"},{"x":-0.0125,"y":1,"name":"(-0.015, -0.01]"},{"x":-0.00749999999999999,"y":4,"name":"(-0.01, -0.00499999999999999]"},{"x":-0.0025,"y":3,"name":"(-0.005, 6.93889390390723e-18]"},{"x":0.0025,"y":2,"name":"(0, 0.005]"},{"x":0.00750000000000001,"y":2,"name":"(0.005, 0.01]"},{"x":0.0125,"y":4,"name":"(0.01, 0.015]"},{"x":0.0175,"y":3,"name":"(0.015, 0.02]"},{"x":0.0225,"y":4,"name":"(0.02, 0.025]"},{"x":0.0275,"y":1,"name":"(0.025, 0.03]"},{"x":0.0325,"y":3,"name":"(0.03, 0.035]"},{"x":0.0375,"y":4,"name":"(0.035, 0.04]"},{"x":0.0425,"y":3,"name":"(0.04, 0.045]"},{"x":0.0475,"y":2,"name":"(0.045, 0.05]"},{"x":0.0525,"y":3,"name":"(0.05, 0.055]"},{"x":0.0575,"y":1,"name":"(0.055, 0.06]"},{"x":0.0625,"y":3,"name":"(0.06, 0.065]"},{"x":0.0675,"y":8,"name":"(0.065, 0.07]"},{"x":0.0725,"y":0,"name":"(0.07, 0.075]"},{"x":0.0775,"y":0,"name":"(0.075, 0.08]"},{"x":0.0825,"y":1,"name":"(0.08, 0.085]"},{"x":0.0875,"y":1,"name":"(0.085, 0.09]"},{"x":0.0925,"y":0,"name":"(0.09, 0.095]"},{"x":0.0975,"y":0,"name":"(0.095, 0.1]"},{"x":0.1025,"y":0,"name":"(0.1, 0.105]"},{"x":0.1075,"y":1,"name":"(0.105, 0.11]"},{"x":0.1125,"y":1,"name":"(0.11, 0.115]"},{"x":0.1175,"y":0,"name":"(0.115, 0.12]"},{"x":0.1225,"y":0,"name":"(0.12, 0.125]"},{"x":0.1275,"y":1,"name":"(0.125, 0.13]"}],"type":"column","pointRange":0.005,"groupPadding":0,"pointPadding":0,"borderWidth":0,"color":"purple"}],"legend":{"enabled":false}},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#ECF0F1"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":["hc_opts.tooltip.formatter"],"jsHooks":[]}</script>

``` r
hc_hist_fun(5, asset_returns, "yellow")
```

<div id="htmlwidget-f81c044bf2f20630d337" style="width:100%;height:500px;" class="highchart html-widget"></div>
<script type="application/json" data-for="htmlwidget-f81c044bf2f20630d337">{"x":{"hc_opts":{"chart":{"reflow":true,"zoomType":"x"},"title":{"text":"XLV Log Returns Distribution"},"yAxis":{"title":{"text":null}},"credits":{"enabled":false},"exporting":{"enabled":true},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0},"treemap":{"layoutAlgorithm":"squarified"}},"tooltip":{"formatter":"function() { return  this.point.name + '<br/>' + this.y; }"},"series":[{"data":[{"x":-0.0975,"y":1,"name":"(-0.1, -0.095]"},{"x":-0.0925,"y":0,"name":"(-0.095, -0.09]"},{"x":-0.0875,"y":0,"name":"(-0.09, -0.085]"},{"x":-0.0825,"y":0,"name":"(-0.085, -0.08]"},{"x":-0.0775,"y":0,"name":"(-0.08, -0.075]"},{"x":-0.0725,"y":1,"name":"(-0.075, -0.07]"},{"x":-0.0675,"y":2,"name":"(-0.07, -0.065]"},{"x":-0.0625,"y":2,"name":"(-0.065, -0.06]"},{"x":-0.0575,"y":0,"name":"(-0.06, -0.055]"},{"x":-0.0525,"y":0,"name":"(-0.055, -0.05]"},{"x":-0.0475,"y":1,"name":"(-0.05, -0.045]"},{"x":-0.0425,"y":0,"name":"(-0.045, -0.04]"},{"x":-0.0375,"y":2,"name":"(-0.04, -0.035]"},{"x":-0.0325,"y":1,"name":"(-0.035, -0.03]"},{"x":-0.0275,"y":3,"name":"(-0.03, -0.025]"},{"x":-0.0225,"y":4,"name":"(-0.025, -0.02]"},{"x":-0.0175,"y":1,"name":"(-0.02, -0.015]"},{"x":-0.0125,"y":0,"name":"(-0.015, -0.01]"},{"x":-0.0075,"y":4,"name":"(-0.01, -0.005]"},{"x":-0.0025,"y":3,"name":"(-0.005, 0]"},{"x":0.0025,"y":3,"name":"(0, 0.005]"},{"x":0.0075,"y":5,"name":"(0.005, 0.01]"},{"x":0.0125,"y":3,"name":"(0.01, 0.015]"},{"x":0.0175,"y":4,"name":"(0.015, 0.02]"},{"x":0.0225,"y":5,"name":"(0.02, 0.025]"},{"x":0.0275,"y":5,"name":"(0.025, 0.03]"},{"x":0.0325,"y":2,"name":"(0.03, 0.035]"},{"x":0.0375,"y":3,"name":"(0.035, 0.04]"},{"x":0.0425,"y":2,"name":"(0.04, 0.045]"},{"x":0.0475,"y":5,"name":"(0.045, 0.05]"},{"x":0.0525,"y":1,"name":"(0.05, 0.055]"},{"x":0.0575,"y":0,"name":"(0.055, 0.06]"},{"x":0.0625,"y":4,"name":"(0.06, 0.065]"},{"x":0.0675,"y":0,"name":"(0.065, 0.07]"},{"x":0.0725,"y":0,"name":"(0.07, 0.075]"},{"x":0.0775,"y":2,"name":"(0.075, 0.08]"},{"x":0.0825,"y":0,"name":"(0.08, 0.085]"},{"x":0.0875,"y":0,"name":"(0.085, 0.09]"},{"x":0.0925,"y":0,"name":"(0.09, 0.095]"},{"x":0.0975,"y":0,"name":"(0.095, 0.1]"},{"x":0.1025,"y":0,"name":"(0.1, 0.105]"},{"x":0.1075,"y":0,"name":"(0.105, 0.11]"},{"x":0.1125,"y":0,"name":"(0.11, 0.115]"},{"x":0.1175,"y":1,"name":"(0.115, 0.12]"}],"type":"column","pointRange":0.005,"groupPadding":0,"pointPadding":0,"borderWidth":0,"color":"yellow"}],"legend":{"enabled":false}},"theme":{"colors":["#f1c40f","#2ecc71","#9b59b6","#e74c3c","#34495e","#3498db","#1abc9c","#f39c12","#d35400"],"chart":{"backgroundColor":"#ECF0F1"},"xAxis":{"gridLineDashStyle":"Dash","gridLineWidth":1,"gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"yAxis":{"gridLineDashStyle":"Dash","gridLineColor":"#BDC3C7","lineColor":"#BDC3C7","minorGridLineColor":"#BDC3C7","tickColor":"#BDC3C7","tickWidth":1},"legendBackgroundColor":"rgba(0, 0, 0, 0.5)","background2":"#505053","dataLabelsColor":"#B0B0B3","textColor":"#34495e","contrastTextColor":"#F0F0F3","maskColor":"rgba(255,255,255,0.3)"},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":["hc_opts.tooltip.formatter"],"jsHooks":[]}</script>
