---
title: "Exploratory Analysis"
author: "MA"
date: "20/12/2022"
output:
  html_document:
    keep_md: true
---

Used libraries


```r
library(tidyverse)
library(knitr)
```


```r
df <- read_csv("C:/Users/admin/Downloads/Drive Data/train_data.csv")
```

Train data dimensions:


```r
dim(df[2:18])
```

```
## [1] 1000000      17
```

Train data variable summary


```r
summary(df[2:18]) %>%
  kable()
```



|   |      id        |      y     |amount_current_loan |    term         |credit_score     |loan_purpose     |yearly_income     |home_ownership   | bankruptcies  |years_current_job | monthly_debt  |years_credit_history |months_since_last_delinquent |open_accounts |credit_problems |credit_balance   |max_open_credit   |
|:--|:---------------|:-----------|:-------------------|:----------------|:----------------|:----------------|:-----------------|:----------------|:--------------|:-----------------|:--------------|:--------------------|:----------------------------|:-------------|:---------------|:----------------|:-----------------|
|   |Min.   :      1 |Min.   :0.0 |Min.   : 10802      |Length:1000000   |Length:1000000   |Length:1000000   |Min.   :    76627 |Length:1000000   |Min.   :0.0000 |Min.   : 0.00     |Min.   :     0 |Min.   : 4.0         |Min.   :  0.0                |Min.   : 0.00 |Min.   : 0.0000 |Min.   :       0 |Min.   :0.000e+00 |
|   |1st Qu.: 250001 |1st Qu.:0.0 |1st Qu.:174394      |Class :character |Class :character |Class :character |1st Qu.:   825797 |Class :character |1st Qu.:0.0000 |1st Qu.: 3.00     |1st Qu.: 10324 |1st Qu.:13.0         |1st Qu.: 16.0                |1st Qu.: 8.00 |1st Qu.: 0.0000 |1st Qu.:  113392 |1st Qu.:2.700e+05 |
|   |Median : 500001 |Median :0.5 |Median :269676      |Mode  :character |Mode  :character |Mode  :character |Median :  1148550 |Mode  :character |Median :0.0000 |Median : 6.00     |Median : 16319 |Median :17.0         |Median : 32.0                |Median :10.00 |Median : 0.0000 |Median :  210539 |Median :4.600e+05 |
|   |Mean   : 500001 |Mean   :0.5 |Mean   :316659      |NA               |NA               |NA               |Mean   :  1344805 |NA               |Mean   :0.1192 |Mean   : 5.88     |Mean   : 18550 |Mean   :18.1         |Mean   : 34.9                |Mean   :11.18 |Mean   : 0.1762 |Mean   :  293847 |Mean   :7.367e+05 |
|   |3rd Qu.: 750000 |3rd Qu.:1.0 |3rd Qu.:435160      |NA               |NA               |NA               |3rd Qu.:  1605899 |NA               |3rd Qu.:0.0000 |3rd Qu.:10.00     |3rd Qu.: 24059 |3rd Qu.:22.0         |3rd Qu.: 51.0                |3rd Qu.:14.00 |3rd Qu.: 0.0000 |3rd Qu.:  367422 |3rd Qu.:7.674e+05 |
|   |Max.   :1000000 |Max.   :1.0 |Max.   :789250      |NA               |NA               |NA               |Max.   :165557393 |NA               |Max.   :7.0000 |Max.   :10.00     |Max.   :435843 |Max.   :70.0         |Max.   :176.0                |Max.   :76.00 |Max.   :15.0000 |Max.   :32878968 |Max.   :1.540e+09 |
|   |NA              |NA          |NA                  |NA               |NA               |NA               |NA's   :219439    |NA               |NA's   :1805   |NA's   :45949     |NA             |NA                   |NA's   :529539               |NA            |NA              |NA               |NA's   :27        |


```r
df$loan_purpose <- as.factor(df$loan_purpose)
df$y <- as.factor(df$y)
```

Summary of character variable - Loan purpose


```r
df %>%
  group_by(loan_purpose) %>%
  summarise(n = n())  %>%
  arrange(desc(n)) %>%
  kable()
```



|loan_purpose         |      n|
|:--------------------|------:|
|debt_consolidation   | 785428|
|other                |  91481|
|home_improvements    |  57517|
|business_loan        |  17756|
|buy_a_car            |  11855|
|medical_bills        |  11521|
|buy_house            |   6897|
|take_a_trip          |   5632|
|major_purchase       |   3727|
|small_business       |   3242|
|moving               |   1548|
|vacation             |   1166|
|wedding              |   1129|
|educational_expenses |    992|
|renewable_energy     |    109|


```r
df %>%
  group_by(y, loan_purpose) %>%
  summarise(n = n()) %>%
  ggplot(aes(fill=y, y=n, x=loan_purpose)) + 
  geom_bar(position="dodge", stat="identity") + 
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  theme_dark()
```

![](report_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Main reasons for taking out a loan:


```r
df %>%
  filter(y == 1) %>%
  group_by(loan_purpose) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  kable()
```



|loan_purpose       |      n|
|:------------------|------:|
|debt_consolidation | 391875|
|other              |  44888|
|home_improvements  |  27274|
|business_loan      |  10356|
|medical_bills      |   6286|
|buy_a_car          |   5810|
|buy_house          |   3652|
|take_a_trip        |   2870|
|small_business     |   2152|
|major_purchase     |   2120|

The number of missing values in each column


```r
na_count <- colSums(is.na(df), na.rm = TRUE)
na_count[na_count > 0]
```

```
##                 credit_score                yearly_income 
##                       314333                       219439 
##                 bankruptcies            years_current_job 
##                         1805                        45949 
## months_since_last_delinquent              max_open_credit 
##                       529539                           27
```

Graphs about Loan purpose for further analysis


```r
library(DT)
df %>%
  group_by(y, loan_purpose) %>%
  summarise(n = n()) %>%
  datatable()
```

```{=html}
<div class="datatables html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-12d4aac14f849bef3598" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-12d4aac14f849bef3598">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"],["0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","1","1","1","1","1","1","1","1"],["buy_a_car","buy_house","business_loan","debt_consolidation","educational_expenses","home_improvements","major_purchase","medical_bills","moving","other","renewable_energy","small_business","take_a_trip","vacation","wedding","buy_a_car","buy_house","business_loan","debt_consolidation","educational_expenses","home_improvements","major_purchase","medical_bills","moving","other","renewable_energy","small_business","take_a_trip","vacation","wedding"],[6045,3245,7400,393553,474,30243,1607,5235,664,46593,42,1090,2762,513,534,5810,3652,10356,391875,518,27274,2120,6286,884,44888,67,2152,2870,653,595]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>y<\/th>\n      <th>loan_purpose<\/th>\n      <th>n<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":3},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```


```r
library(plotly)
df %>%
  group_by(y, credit_score) %>%
  summarise(n = n()) %>%
  plot_ly(x = ~credit_score, y = ~n, name = ~y, type = "bar")
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-9c8bfe1bdefe70157025" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-9c8bfe1bdefe70157025">{"x":{"visdat":{"258457c77fe":["function () ","plotlyVisDat"]},"cur_data":"258457c77fe","attrs":{"258457c77fe":{"x":{},"y":{},"name":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":"credit_score","type":"category","categoryorder":"array","categoryarray":["fair","good","very_good"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"n"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["fair","good","very_good"],"y":[26261,276478,91270],"name":"0","type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["fair","good","very_good"],"y":[33518,216691,41449],"name":"1","type":"bar","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```
