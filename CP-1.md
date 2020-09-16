---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.2
```

```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 4.0.2
```

```
## -- Attaching packages ----------------------------------------------------------- tidyverse 1.3.0 --
```

```
## v tibble  3.0.1     v dplyr   1.0.0
## v tidyr   1.1.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
## v purrr   0.3.4
```

```
## Warning: package 'tibble' was built under R version 4.0.2
```

```
## Warning: package 'tidyr' was built under R version 4.0.2
```

```
## Warning: package 'readr' was built under R version 4.0.2
```

```
## Warning: package 'purrr' was built under R version 4.0.2
```

```
## Warning: package 'forcats' was built under R version 4.0.2
```

```
## -- Conflicts -------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
png("mean.png")
hist(activity$steps, col = "blue")
abline(v = mean(activity$steps, na.rm = TRUE), lwd = 2, col = "red")
abline(v = median(activity$steps, na.rm = TRUE), lwd = 2, col = "green")
dev.off()
```

```
## png 
##   2
```

```r
meanACT <- mean(activity$steps, na.rm = TRUE)
medACT <- median(activity$steps, na.rm = TRUE)
```
The mean and median values of total number of steps taken per day are **37.3825996** and **0** respectively.

## What is the average daily activity pattern?

```r
activity$date <- as.Date(activity$date)
reqdf <- activity %>% group_by(interval) %>% summarise(steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
max <- which.max(reqdf$steps)
max_value <- as.numeric(reqdf[max,1])
png("average.png")
theme_set(theme_light())
g <- ggplot(data = reqdf, aes(interval, steps))
g + geom_line(color = "red", size = 1) + geom_vline(xintercept = max_value, size = 0.8, color = "blue") + geom_text(aes(x = max_value, label = max_value), y = 0.1, angle = 90, vjust = -0.2)
dev.off()
```

```
## png 
##   2
```
The **835** second interval contains the maximum number of steps.

## Imputing missing values

```r
NArows <- which(is.na(activity))
NAdf <- cbind(activity, mean = as.numeric(reqdf$steps))
for (i in NArows) {
  NAdf[i, 1] <- NAdf[i, 4]
}
NEWdf <- NAdf[, 1:3]
png("plot3.png")
hist(NEWdf$steps, col = "blue")
abline(v = mean(NEWdf$steps), lwd = 2, col = "red")
meanNEW <- mean(NEWdf$steps)
abline(v = median(NEWdf$steps,), lwd = 2, col = "green")
medNEW <- median(NEWdf$steps)
dev.off()
```

```
## png 
##   2
```
The total number of missing values are **2304**. The mean and median values are equal to the initial values.

## Are there differences in activity patterns between weekdays and weekends?

```r
week <- weekdays(activity$date)
week <- as.factor(week)
levels(week) <- c("weekday", "weekday","weekend", "weekend","weekday","weekday","weekday")
finaldf <- cbind(NEWdf, week)
png("plot4.png")
gg <- ggplot(data = finaldf, aes(interval, steps)) 
gg + geom_line() + facet_grid(levels(week)~.)
dev.off() 
```

```
## png 
##   2
```
The shape of the plot seems to be retained in both cases.
