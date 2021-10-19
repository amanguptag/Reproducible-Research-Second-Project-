---
title: "Human and Economic Costs of Severe Weather Events"
output:
  html_document:
    keep_md: yes
---

## Synopsis

The NOAA records data on U.S. weather. Storms and other extreme weather events are reported along with their associated effects on human life, injury, property, and crops. In this report, we explore the data from 1950--2011 and determine those weather events that have caused the most harm in terms of human and economic costs.

## Data Processing

We load the `dplyr` package for data manipulation, the `lubridate` package for working with dates, and the `ggplot2` package for graphing. We will also load Winston Chang's helpful `multiplot` function:


```r
library("dplyr")
library("lubridate")
library("ggplot2")
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
```


Although the data comes in a compressed format, `read.csv` has no trouble parsing it.


```r
storm_data_raw <- read.csv("repdata-data-StormData.csv.bz2",
                           stringsAsFactors = FALSE)
```

We will `select` only the columns that pertain to our questions:

* `EVTYPE`: the event type,
* `BGN_DATE`: the date of the event,
* `FATALITIES`: the number of fatalities that resulted,
* `INJURIES`: the number of injuries that resulted,
* `PROPDMG`: an estimate for the amount of property damage,
* `PROPDMGEXP`: a "multiplier" extension for the amount in `PROPDMG`,
* `CROPDMG`: an estimate for the amount of crop damage,
* `CROPDMGEXP`: a "multiplier" extension for the amount in `CROPDMG`,
* `REMARKS`: remarks added by the person recording the data.


```r
storm_data <- storm_data_raw %>%
    select(EVTYPE, BGN_DATE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP,
           CROPDMG, CROPDMGEXP, REMARKS)
```

First, we'll deal with the `BGN_DATE` variable. As we're not concerned about the time of day (and it's not even recorded in many cases), we will extract only the date.


```r
date <- storm_data$BGN_DATE %>%
    strsplit(" ") %>%
    unlist %>%
    matrix(nrow = length(storm_data$BGN_DATE), ncol = 2, byrow = TRUE)
date <- as.Date(date[,1], format = "%m/%d/%Y")
storm_data <- storm_data %>%
    mutate(date = date)
rm(date)
```


Next, we'll calculate the property and crop damage in dollars. In the instructions for storm data preparation (located [here](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)), we read,

>Estimates should be rounded to three significant digits, followed by an alphabetical character signifying the magnitude of the number, i.e., 1.55B for $1,550,000,000. Alphabetical characters used to signify magnitude include “K” for thousands, “M” for millions, and “B” for billions.

Therefore, to get accurate damage estimates, we need to multiply our damages by the appropriate factor.

The bad news is that there is some messiness in the variables `PROPDMGEXP` and `CROPDMGEXP`.


```r
table(storm_data$PROPDMGEXP)
```

```
## 
##             -      ?      +      0      1      2      3      4      5      6 
## 465934      1      8      5    216     25     13      4      4     28      4 
##      7      8      B      h      H      K      m      M 
##      5      1     40      1      6 424665      7  11330
```

```r
table(storm_data$CROPDMGEXP)
```

```
## 
##             ?      0      2      B      k      K      m      M 
## 618413      7     19      1      9     21 281832      1   1994
```

The good news is that there aren't many too many cases that have strange entries in this field. One possibiility is that the numerical digits may have been recorded by people assuming that this field was for the power of ten required for scientific notation (i.e., $3000 = 3.00 \times 10^{3}$). This would be especially problematic for our analysis for large powers of 10, indicating dollar figures possibly in the millions or more. We examine these specific entries for clues:


```r
storm_data %>%
    filter(PROPDMGEXP %in% 5:8) %>%
    select(EVTYPE, PROPDMG, PROPDMGEXP) %>%
    arrange(PROPDMGEXP)
```

```
##                         EVTYPE PROPDMG PROPDMGEXP
## 1  URBAN/SMALL STREAM FLOODING     0.0          5
## 2                   HEAVY SNOW     1.7          5
## 3                         HAIL    30.0          5
## 4           THUNDERSTORM WINDS     0.1          5
## 5           THUNDERSTORM WINDS     0.0          5
## 6           THUNDERSTORM WINDS    10.0          5
## 7                      TORNADO    14.0          5
## 8           THUNDERSTORM WINDS    12.0          5
## 9                      TORNADO    88.0          5
## 10          THUNDERSTORM WINDS    12.0          5
## 11                   LIGHTNING     3.0          5
## 12                   LIGHTNING     0.2          5
## 13                 FLASH FLOOD    12.0          5
## 14          THUNDERSTORM WINDS     1.0          5
## 15                   LIGHTNING     1.0          5
## 16                    FLOODING     5.2          5
## 17                        HAIL     0.0          5
## 18                        HAIL     0.0          5
## 19                        HAIL     0.0          5
## 20                        HAIL     0.0          5
## 21                        HAIL     0.0          5
## 22                        HAIL     0.0          5
## 23                        HAIL     0.0          5
## 24          THUNDERSTORM WINDS     0.0          5
## 25                     TORNADO     0.2          5
## 26                    FLOODING     0.7          5
## 27                   LIGHTNING    13.0          5
## 28                 FLASH FLOOD     6.4          5
## 29          THUNDERSTORM WINDS    24.0          6
## 30          THUNDERSTORM WINDS    26.0          6
## 31          THUNDERSTORM WINDS    15.0          6
## 32                     TORNADO     0.0          6
## 33          THUNDERSTORM WINDS    14.0          7
## 34                 FLASH FLOOD    68.0          7
## 35                     TORNADO     0.0          7
## 36                        HAIL     0.0          7
## 37                   LIGHTNING     0.0          7
## 38                        HAIL     0.0          8
```


To begin with, the multipliers will make no difference to a bunch of entries where the damage in `PROPDMG` is recorded as 0.0, even though it is clear in most of the accompanying remarks that there was, indeed, damage. (I have not printed the remarks here because they are not fomatted very nicely.) But even for those cases with nonzero figures, the remarks are not easy to reconcile to the figures. For example, the entry labeled `## 2` in the output above mentions several destroyed buildings including a home valued at $250,000 to $300,000. However, the propery damage is recorded as PROPDMG = 1.7 and PROPDMGEXP = 5. Note that $1.7 \times 10^5$ is 170,000, nowhere near high enough. Similar problems exist in trying to reconcile most of the other damage figures above with remarks that seem off by orders of magnitude.

Given the relatively small number of cases involved here, it seems best to ignore the mysterious entries and focus on the cases with "k", "K", "m", "M", and "B". (Anything else will get assigned to 1 so that the multiplier causes no change in the value.) We will do this with `PROPDMG` and `CROPDMG`.


```r
PROPDMGEXP_value <- sapply(storm_data$PROPDMGEXP,
    function(x) {switch(x,  "k" = 1000, "K" = 1000,
                            "m" = 1000000, "M" = 1000000, 
                            "B" = 1000000000, 1)})

CROPDMGEXP_value <- sapply(storm_data$CROPDMGEXP,
    function(x) {switch(x,  "k" = 1000, "K" = 1000,
                            "m" = 1000000, "M" = 1000000, 
                            "B" = 1000000000, 1)})
storm_data <- storm_data %>%
    mutate(PROPDMG_value = PROPDMGEXP_value,
           CROPDMG_value = CROPDMGEXP_value,
           PROPDMG_actual = PROPDMG * PROPDMG_value,
           CROPDMG_actual = CROPDMG * CROPDMG_value)
rm(PROPDMGEXP_value)
rm(CROPDMGEXP_value)
```


## Results

### 1. Across the United States, which types of events (as indicated in the `EVTYPE` variable) are most harmful with respect to population health?

First, we get totals for all the quantities of interest.


```r
storm_impact <- storm_data %>%
    group_by(EVTYPE) %>%
    summarize(Fatalities = sum(FATALITIES),
              Injuries = sum(INJURIES),
              Property_damage = sum(PROPDMG_actual),
              Crop_damage = sum(CROPDMG_actual))
```


Simple tables suffice here. We look at the top ten events that have caused the most fatalities and injuries respectively.


```r
storm_impact %>%
    select(EVTYPE, Fatalities) %>%
    arrange(desc(Fatalities)) %>%
    head(10) %>%
    format(big.mark = ",") %>%
    knitr::kable("markdown", align = c('l', 'r'))
```



|x                            |
|:----------------------------|
|# A tibble: 10 x 2           |
|EVTYPE         Fatalities    |
|<chr>               <dbl>    |
|1 TORNADO              5633  |
|2 EXCESSIVE HEAT       1903  |
|3 FLASH FLOOD           978  |
|4 HEAT                  937  |
|5 LIGHTNING             816  |
|6 TSTM WIND             504  |
|7 FLOOD                 470  |
|8 RIP CURRENT           368  |
|9 HIGH WIND             248  |
|10 AVALANCHE             224 |

```r
storm_impact %>%
    select(EVTYPE, Injuries) %>%
    arrange(desc(Injuries)) %>%
    head(10) %>%
    format(big.mark = ",") %>%
    knitr::kable("markdown", align = c('l', 'r'))
```



|x                             |
|:-----------------------------|
|# A tibble: 10 x 2            |
|EVTYPE            Injuries    |
|<chr>                <dbl>    |
|1 TORNADO              91346  |
|2 TSTM WIND             6957  |
|3 FLOOD                 6789  |
|4 EXCESSIVE HEAT        6525  |
|5 LIGHTNING             5230  |
|6 HEAT                  2100  |
|7 ICE STORM             1975  |
|8 FLASH FLOOD           1777  |
|9 THUNDERSTORM WIND     1488  |
|10 HAIL                  1361 |

We can see that there is some overlap in the categories. For example, one could argue that "HEAT" and "EXCESSIVE HEAT" could be counted as one category for purposes of assessing impact on human suffering. "THUNDERSTORM WIND" and "TSTM WIND" are exactly the same thing. (Some data recorders abbreviated and others did not.) Nevertheless, even when combining like categories, it is clear that tornadoes are by far the most destructive natural force when it comes to safety and human life.


```r
tornadoes <- storm_data %>%
    filter(EVTYPE == "TORNADO") %>%
    group_by(Year = year(date)) %>%
    summarize(Fatalities = sum(FATALITIES), Injuries = sum(INJURIES))
g1 <- ggplot(tornadoes, aes(x = Year, y = Fatalities)) +
    ggtitle("Tornadoes in the U.S. (1950-2011)") +
    xlab(NULL) +
    scale_x_continuous(breaks=seq(1950,2011,10)) +
    geom_line()
g2 <- ggplot(tornadoes, aes(x = Year, y = Injuries)) +
    scale_x_continuous(breaks=seq(1950,2011,10)) +
    geom_line()
multiplot(g1, g2)
```

![](repdata-005_Project_2_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

The figure shows two interesting things. One is that there are specific years in which tornadoes are particularly devastating. Note that 2011 was one of the worst years on record. (See [this Wikipedia page](https://en.wikipedia.org/wiki/April_25%E2%80%9328,_2011_tornado_outbreak#April_27_event).) The other observation is that there is a clear change in the magnitude of the effect in the more severe years before and after the 1970s. This is due to the widespread adoption around 1970 of using air raid sirens as tornado warnings. (See the [paper by Coleman et al.](http://journals.ametsoc.org/doi/pdf/10.1175/2010BAMS3062.1))

### 2. Across the United States, which types of events have the greatest economic consequences?

We look at similar tables for property damage and crop damage (in dollars) to assess economic impact.


```r
options(scipen = 10)
storm_impact %>%
    select(EVTYPE, Property_damage) %>%
    arrange(desc(Property_damage)) %>%
    head(10) %>%
    format(big.mark = ",") %>%
    knitr::kable("markdown", align = c('l', 'r'))
```



|x                                   |
|:-----------------------------------|
|# A tibble: 10 x 2                  |
|EVTYPE            Property_damage   |
|<chr>                       <dbl>   |
|1 FLOOD               144657709807  |
|2 HURRICANE/TYPHOON    69305840000  |
|3 TORNADO              56937160779. |
|4 STORM SURGE          43323536000  |
|5 FLASH FLOOD          16140812067. |
|6 HAIL                 15732267048. |
|7 HURRICANE            11868319010  |
|8 TROPICAL STORM        7703890550  |
|9 WINTER STORM          6688497251  |
|10 HIGH WIND             5270046295 |

```r
storm_impact %>%
    select(EVTYPE, Crop_damage) %>%
    arrange(desc(Crop_damage)) %>%
    head(10) %>%
    format(big.mark = ",") %>%
    knitr::kable("markdown", align = c('l', 'r'))
```



|x                                |
|:--------------------------------|
|# A tibble: 10 x 2               |
|EVTYPE            Crop_damage    |
|<chr>                   <dbl>    |
|1 DROUGHT           13972566000  |
|2 FLOOD              5661968450  |
|3 RIVER FLOOD        5029459000  |
|4 ICE STORM          5022113500  |
|5 HAIL               3025954473  |
|6 HURRICANE          2741910000  |
|7 HURRICANE/TYPHOON  2607872800  |
|8 FLASH FLOOD        1421317100  |
|9 EXTREME COLD       1292973000  |
|10 FROST/FREEZE       1094086000 |

There is a difference in the weather event taking the top spot for property damage versus crop damage.  Floods cause more than double the amount of property damage than the next highest event (hurricane/typhoon). Flooding also causes a lot of crop damage, but drought is the most deleterious.
