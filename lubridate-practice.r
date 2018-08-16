# Following along with "R for Data Science", chapter 13
#load stuff
install.packages("lubridate")
install.packages("nycflights13")
library(lubridate)
library(tidyverse)
library(nycflights13)

#samples
ymd(c("2010-10-10", "bananas", "2018-05-05", "apples"))
today()
?today
mdy("January 1, 2010")
ymd("2015-Mar-07")
mdy(c("August 19 (2015)", "July 1 (2015"))
mdy("12/30/14")
now()

as_date(now())
as_datetime(today())

wday(today())
wday(today(), label=TRUE, abbr=FALSE)

# prep flights data set for fun graphing, including custom function
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights
flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time), 
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))

# graph of number of flights departed each day
flights_dt %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

#departure counts by day of week
flights_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>%
  ggplot(aes(wday)) +
  geom_bar()

# departure counts by week
flights_dt %>%
  count(week = floor_date(dep_time, "week")) %>%
  ggplot(aes(week, n)) +
  geom_line() 
