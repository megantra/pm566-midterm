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
```

``` r
if (!file.exists("ofp-ccw-by-race-ethn_contra-type_age-group_14-16.csv")) {
download.file("https://data.chhs.ca.gov/dataset/c2698502-d276-4e55-9057-8153e39d21b1/resource/12a73f54-dcf4-4e38-843c-e988385be69b/download/ofp-ccw-by-race-ethn_contra-type_age-group_14-16.csv", "ofp-ccw-by-race-ethn_contra-type_age-group_14-16.csv", method="libcurl", timeout = 60) 
}
contra <- data.table::fread("ofp-ccw-by-race-ethn_contra-type_age-group_14-16.csv") 
```

\##Introduction (provide background on your dataset and formulated
question)

The dataset was compiled for the Measure CCW, Contraceptive Care as part
of the Maternal and Infant Health Initiative, Contraceptive Care Quality
grant.

Question: Are older women (age 21-44) more likely to use Long-Acting
Reversible Contraceptives than younger women (age 15-20) and is the
trend consistent across all races?

\##Methods (include how and where the data were acquired, how you
cleaned and wrangled the data, what tools you used for data exploration)

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

\##Preliminary Results (provide summary statistics in tabular form and
publication-quality figures, take a look at the kable function from
knitr to write nice tables in Rmarkdown)

\##Conclusion about what you found in terms of the formulated question.
