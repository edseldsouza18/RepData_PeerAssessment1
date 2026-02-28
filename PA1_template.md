---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

``` r
# Load required library
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
# Read the dataset
activity <- read.csv("activity.csv")

# Convert date column to Date class
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

# Verify structure
print(head(activity))
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

``` r
dim(activity)
```

```
## [1] 17568     3
```

## What is mean total number of steps taken per day?

``` r
# Calculate total steps per day
daily_steps <- activity %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))

# Compute mean and median (ignoring NA days)
mean_steps <- mean(daily_steps$total_steps, na.rm = TRUE)
median_steps <- median(daily_steps$total_steps, na.rm = TRUE)

print(mean_steps)
```

```
## [1] 9354.23
```

``` r
median_steps
```

```
## [1] 10395
```

## What is the average daily activity pattern?

``` r
# Compute average steps per 5-minute interval (across all days)
avg_interval <- activity %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

# Find interval with maximum average steps
max_interval <- avg_interval[which.max(avg_interval$avg_steps), ]
max_interval
```

```
## # A tibble: 1 × 2
##   interval avg_steps
##      <int>     <dbl>
## 1      835      206.
```
##Imputing missing values

``` r
# Count NAs
sum(is.na(activity$steps))
```

```
## [1] 2304
```

``` r
# Strategy: Replace NA steps with mean for that 5-minute interval
activity_imputed <- activity %>%
  left_join(avg_interval, by = "interval") %>%
  mutate(steps = ifelse(is.na(steps), round(avg_steps), steps)) %>%
  select(-avg_steps)

# Verify no NAs remain
sum(is.na(activity_imputed$steps))
```

```
## [1] 0
```

## Are there differences in activity patterns between weekdays and weekends?

``` r
# Add weekday/weekend factor
activity_imputed <- activity_imputed %>%
  mutate(
    day = weekdays(date),
    day_type = ifelse(day %in% c("Saturday", "Sunday"), "weekend", "weekday")
  )

# Average steps per interval by day type
daytype_pattern <- activity_imputed %>%
  group_by(day_type, interval) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` has regrouped the output.
## ℹ Summaries were computed grouped by day_type and interval.
## ℹ Output is grouped by day_type.
## ℹ Use `summarise(.groups = "drop_last")` to silence this message.
## ℹ Use `summarise(.by = c(day_type, interval))` for per-operation grouping
##   (`?dplyr::dplyr_by`) instead.
```

``` r
# Preview
head(daytype_pattern)
```

```
## # A tibble: 6 × 3
## # Groups:   day_type [1]
##   day_type interval avg_steps
##   <chr>       <int>     <dbl>
## 1 weekday         0    2.29  
## 2 weekday         5    0.4   
## 3 weekday        10    0.156 
## 4 weekday        15    0.178 
## 5 weekday        20    0.0889
## 6 weekday        25    1.58
```
