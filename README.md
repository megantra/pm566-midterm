Midterm Project
================
Megan Tran
October 23, 2022

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ lubridate::as.difftime() masks base::as.difftime()
    ## ✖ lubridate::date()        masks base::date()
    ## ✖ dplyr::filter()          masks stats::filter()
    ## ✖ lubridate::intersect()   masks base::intersect()
    ## ✖ dplyr::lag()             masks stats::lag()
    ## ✖ lubridate::setdiff()     masks base::setdiff()
    ## ✖ lubridate::union()       masks base::union()

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year

``` r
library(ggplot2)
library(dtplyr)
library(dplyr)
library(stringr)
```

``` r
if (!file.exists("ofp-ccw-by-race-ethn_contra-type_age-group_14-16.csv")) {
download.file("https://data.chhs.ca.gov/dataset/c2698502-d276-4e55-9057-8153e39d21b1/resource/12a73f54-dcf4-4e38-843c-e988385be69b/download/ofp-ccw-by-race-ethn_contra-type_age-group_14-16.csv", "ofp-ccw-by-race-ethn_contra-type_age-group_14-16.csv", method="libcurl", timeout = 60) 
}
contra <- data.table::fread("ofp-ccw-by-race-ethn_contra-type_age-group_14-16.csv") 
```

\##Introduction (provide background on your dataset and formulated
question)

The Contraceptive Care - All Women measure (CCW), as part of the
Maternal and Infant Health Initiative, Contraceptive Care Quality grant,
was compiled data from all women ages 15-44 at risk for unintended
pregnancy. The women were stratified in two age groups, those who are
15-20 and those who are 21-44. These women were either receiving
long-acting reversible methods of contraception (LARC) or
most/moderately effective methods of contraceptions (M/M), like female
sterilization, contraceptive implants, IUDs, oral pills, patches, rings,
injectables, or diaphragms. Data was gathered for 3 consecutive years,
2014-2016, in California.

The research question being explore is if younger women (age 15-20) are
more likely to use most/moderately effective methods of contraceptions
(\</M) than older women (age 21-44) and is the trend consistent the
three year period?

\##Methods (include how and where the data were acquired, how you
cleaned and wrangled the data, what tools you used for data exploration)

The data was collected through administrative survey measures. The
representative sample excluded women not at risk of unintended pregnancy
because they were infecund for non-contraceptive reasons, had a live
birth in the last 2 months of the measurement year, or were pregnant or
their pregnancy outcome was unknown at the end of the year(s). Once the
exclusions were applied, the sample included women who were not pregnant
at any point in the 3-year period, those who had a live birth in the
first 10 months of the measurement year(s), and those who had a
miscarriage, stillbirth, ectopic pregnancy, or induced abortion.

``` r
mean(is.na(contra))
```

    ## [1] 0

``` r
str(contra)
```

    ## Classes 'data.table' and 'data.frame':   72 obs. of  7 variables:
    ##  $ Year                     : int  2014 2014 2014 2014 2014 2014 2014 2014 2014 2014 ...
    ##  $ Age Group                : chr  "15-20 year olds" "15-20 year olds" "15-20 year olds" "15-20 year olds" ...
    ##  $ Race / Ethnicity         : chr  "Hispanic" "White" "Black" "Asian" ...
    ##  $ Contraceptive Type       : chr  "Most/Moderately Effective" "Most/Moderately Effective" "Most/Moderately Effective" "Most/Moderately Effective" ...
    ##  $ Eligible                 : int  207606 60043 31795 82045 2902 139084 207606 60043 31795 82045 ...
    ##  $ Contraceptive Use        : int  97402 26771 13348 24495 1108 43198 21139 5892 2907 5614 ...
    ##  $ Rate of Contraceptive Use: chr  "46.92%" "44.59%" "41.98%" "29.86%" ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
contra$`Rate of Contraceptive Use` <- stringr::str_remove_all(contra$`Rate of Contraceptive Use`, "%")
```

``` r
contra$`Rate of Contraceptive Use` <- as.numeric(contra$`Rate of Contraceptive Use`)
```

``` r
avg_contra <- contra[ , .(
    avg_rate = mean (`Rate of Contraceptive Use`)
  ), 
  by = .(`Contraceptive Type`, `Age Group`, Year)]
```

\##Preliminary Results (provide summary statistics in tabular form and
publication-quality figures, take a look at the kable function from
knitr to write nice tables in Rmarkdown)

``` r
contra[!is.na(`Rate of Contraceptive Use`)] %>% 
  ggplot()+
  geom_boxplot(mapping=aes(x=`Contraceptive Type`, y= `Rate of Contraceptive Use`, fill = `Contraceptive Type`)) +
    facet_wrap(Year ~ `Age Group`, nrow=3)
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
database <- data.frame(
    Year = avg_contra$Year, 
   Contraceptive_Type = avg_contra$`Contraceptive Type`,
    Age_Group = avg_contra$`Age Group`,
   Average_Rate_of_Contraceptive_Use = avg_contra$avg_rate
 )
 knitr::kable(database, caption = "Contraceptive Use of Women 2014-2016")
```

| Year | Contraceptive_Type        | Age_Group       | Average_Rate_of_Contraceptive_Use |
|-----:|:--------------------------|:----------------|----------------------------------:|
| 2014 | Most/Moderately Effective | 15-20 year olds |                         38.765000 |
| 2014 | LARC                      | 15-20 year olds |                          8.635000 |
| 2014 | Most/Moderately Effective | 21-44 year olds |                         42.683333 |
| 2014 | LARC                      | 21-44 year olds |                          8.290000 |
| 2015 | LARC                      | 21-44 year olds |                          6.785714 |
| 2015 | Most/Moderately Effective | 15-20 year olds |                         32.613333 |
| 2015 | LARC                      | 15-20 year olds |                          5.605000 |
| 2015 | Most/Moderately Effective | 21-44 year olds |                         31.471667 |
| 2016 | Most/Moderately Effective | 15-20 year olds |                         31.408333 |
| 2016 | LARC                      | 15-20 year olds |                          5.156667 |
| 2016 | Most/Moderately Effective | 21-44 year olds |                         30.286667 |
| 2016 | LARC                      | 21-44 year olds |                          6.576667 |

Contraceptive Use of Women 2014-2016

\##Conclusion about what you found in terms of the formulated question.
