library(tidyverse)
library(rvest)
library(readxl)
library(janitor)
library(summarytools)
library(forecast)
library(tidyquant)
library(timetk)
library(sweep)

setwd("~/tidytuesday/data/2019/2019-02-05")
state_hpi <- readr::read_csv("state_hpi.csv")
mortgage_rates <- read.csv("mortgage.csv")
recession_dates <- readr::read_csv("recessions.csv")


#super handy package for doing a quick summary table
view(dfSummary(state_hpi))
view(dfSummary(mortgage_rates))
view(dfSummary(recession_dates))


g.line <- ggplot(data = mortgage_rates,
                 aes(x=mortgage_rates$date, y=mortgage_rates$fixed_rate_30_yr, ymax = fixed_rate_30_yr)) +
  geom_line()

#too many dates duh need to aggregate

mortgage_rates_monthly <- mortgage_rates %>%
  mutate(month = month(date, label = TRUE),
         year  = year(date)) %>%
  group_by(year, month) %>%
  summarise(average.rate = mean(fixed_rate_30_yr))


g.line.mean <- ggplot(data = mortgage_rates_monthly,
                 aes(x=mortgage_rates_monthly$year, y=mortgage_rates_monthly$average.rate, ymax = average.rate)) +
  geom_line()+
  theme_minimal()+
  scale_y_continuous(breaks=NULL,sec.axis=dup_axis())




