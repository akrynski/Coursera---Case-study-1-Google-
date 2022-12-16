### Divvy_Exercise_Full_Year_Analysis ###


# This analysis is based on the Divvy case study "'Sophisticated, Clear,
#and Polished’: Divvy and Data Visualization" written by Kevin Hartman
#(found here: https://artscience.blog/home/divvy-dataviz-case-study).
#The purpose of this script is to consolidate downloaded Divvy data into
#a single dataframe and then conduct simple analysis to help answer
#the key question:
#“In what ways do members and casual riders use Divvy bikes differently?”


# # # # # # # # # # # # # # # # # # # # # # #
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #
install.packages("tidyverse")

install.packages("skimr")
install.packages("here")
install.packages("janitor")
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(here)
library(skimr)
library(janitor)
getwd() #displays your working directory
setwd("D:/R/CASE STUDY - 1/data")
#sets your working directory to simplify calls to data ...
#make sure to use your OWN username instead of mine ;)


#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
april20      <- read.csv("202004-divvy-tripdata.csv")
may20        <- read.csv("202005-divvy-tripdata.csv")
juni20       <- read.csv("202006-divvy-tripdata.csv")
jully20      <- read.csv("202006-divvy-tripdata.csv")
august20     <- read.csv("202008-divvy-tripdata.csv")
september20  <- read.csv("202009-divvy-tripdata.csv")
october20    <- read.csv("202010-divvy-tripdata.csv")
november20   <- read.csv("202011-divvy-tripdata.csv")
december20   <- read.csv("202012-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files  (verifing the data’s integrity task)
# While the names don't have to be in the same order, they DO need to match
# perfectly before we can use a command to join them into one file
a <- colnames(april20) == colnames(may20)
b <- colnames(juni20) == colnames(jully20)
c <- colnames(august20) == colnames(september20)
d <- colnames(october20) == colnames(november20)
e <- colnames(november20) == colnames(december20)
a
b
c
d
e

colnames(april20)
colnames(may20)
colnames(juni20)
colnames(jully20)
colnames(august20)
colnames(september20)
colnames(october20)
colnames(november20)
colnames(december20)
# RESULT: column names are equal and don't need any renaming
#===============================================================================
# check for data type
year_datas <-
  list(
    april20,
    may20,
    juni20,
    jully20,
    august20,
    september20,
    october20,
    november20,
    december20
  )
str(year_datas)
#!!! december20 has start_station_id and end_station_if of type char instead of int !!!
december20 <-
  mutate(
    december20,
    start_station_id = as.integer(start_station_id)
    ,
    end_station_id = as.integer(end_station_id)
  )

#!!! after convertion most fields are NA !!!
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#!!! all months have columns started_at and ended_at are of type str instead of datetime
# I'll change it to POSIXct datetime format,then
# calculate the ride_length (in the meaning of trip duration)
# !!! IN DECEMBER THE MEAN VALUE IS NEGATIVE !!!
# some values of ended_at are lower than values of started_at - that's the reason

# I've decided to change all <0 values of difference between ended_at and started_at 
# columns into zero (0). The reason is that we have no way to prove if the columns 
# were mistaken or if the values in it are wrong typed.
april20 <-
  mutate(april20,
         started_at = as.POSIXct(started_at),
         ended_at = as.POSIXct(ended_at),
         trip_duration = ifelse((as.numeric(round((ended_at - started_at)/60, digit = 0))) < 0,0,(as.numeric(round((ended_at - started_at)/60, digit = 0)))),
         trip_wday = wday(started_at,label=TRUE, abbr=FALSE),
         isweekend = ifelse(is.element(trip_wday,c("sobota","niedziela")),"weekend","weekday")
  )
may20 <-
  mutate(
    may20,
    started_at = as.POSIXct(started_at),
    ended_at = as.POSIXct(ended_at),
    trip_duration = ifelse((as.numeric(round((ended_at - started_at)/60, digit = 0))) < 0,0,(as.numeric(round((ended_at - started_at)/60, digit = 0)))),
    trip_wday = wday(started_at,label=TRUE, abbr=FALSE),
    isweekend = ifelse(is.element(trip_wday,c("sobota","niedziela")),"weekend","weekday")
  )
juni20 <-
  mutate(
    juni20,
    started_at = as.POSIXct(started_at),
    ended_at = as.POSIXct(ended_at),
    trip_duration = ifelse((as.numeric(round((ended_at - started_at)/60, digit = 0))) < 0,0,(as.numeric(round((ended_at - started_at)/60, digit = 0)))),
    trip_wday = wday(started_at,label=TRUE, abbr=FALSE),
    isweekend = ifelse(is.element(trip_wday,c("sobota","niedziela")),"weekend","weekday")
  )
jully20 <-
  mutate(
    jully20,
    started_at = as.POSIXct(started_at),
    ended_at = as.POSIXct(ended_at),
    trip_duration = ifelse((as.numeric(round((ended_at - started_at)/60, digit = 0))) < 0,0,(as.numeric(round((ended_at - started_at)/60, digit = 0)))),
    trip_wday = wday(started_at,label=TRUE, abbr=FALSE),
    isweekend = ifelse(is.element(trip_wday,c("sobota","niedziela")),"weekend","weekday")
  )
august20 <-
  mutate(
    august20,
    started_at = as.POSIXct(started_at),
    ended_at = as.POSIXct(ended_at),
    trip_duration = ifelse((as.numeric(round((ended_at - started_at)/60, digit = 0))) < 0,0,(as.numeric(round((ended_at - started_at)/60, digit = 0)))),
    trip_wday = wday(started_at,label=TRUE, abbr=FALSE),
    isweekend = ifelse(is.element(trip_wday,c("sobota","niedziela")),"weekend","weekday")
  )
september20 <-
  mutate(
    september20,
    started_at = as.POSIXct(started_at),
    ended_at = as.POSIXct(ended_at),
    trip_duration = ifelse((as.numeric(round((ended_at - started_at)/60, digit = 0))) < 0,0,(as.numeric(round((ended_at - started_at)/60, digit = 0)))),
    trip_wday = wday(started_at,label=TRUE, abbr=FALSE),
    isweekend = ifelse(is.element(trip_wday,c("sobota","niedziela")),"weekend","weekday")
  )
october20 <-
  mutate(
    october20,
    started_at = as.POSIXct(started_at),
    ended_at = as.POSIXct(ended_at),
    trip_duration = ifelse((as.numeric(round((ended_at - started_at)/60, digit = 0))) < 0,0,(as.numeric(round((ended_at - started_at)/60, digit = 0)))),
    trip_wday = wday(started_at,label=TRUE, abbr=FALSE),
    isweekend = ifelse(is.element(trip_wday,c("sobota","niedziela")),"weekend","weekday")
  )
november20 <-
  mutate(
    november20,
    started_at = as.POSIXct(started_at),
    ended_at = as.POSIXct(ended_at),
    trip_duration = ifelse((as.numeric(round((ended_at - started_at)/60, digit = 0))) < 0,0,(as.numeric(round((ended_at - started_at)/60, digit = 0)))),
    trip_wday = wday(started_at,label=TRUE, abbr=FALSE),
    isweekend = ifelse(is.element(trip_wday,c("sobota","niedziela")),"weekend","weekday")
  )
december20 <-
  mutate(
    december20,
    started_at = as.POSIXct(started_at),
    ended_at = as.POSIXct(ended_at),
    trip_duration = ifelse((as.numeric(round((ended_at - started_at)/60, digit = 0))) < 0,0,(as.numeric(round((ended_at - started_at)/60, digit = 0)))),
    trip_wday = wday(started_at,label=TRUE, abbr=FALSE),
    isweekend = ifelse(is.element(trip_wday,c("sobota","niedziela")),"weekend","weekday")
  )

#------------------------------------------------
#            Calculate the mean of ride_length (in the meaning of trip duration)

sel_april20 <- april20 %>%
  clean_names() %>%
  group_by(member_casual) %>%
  summarise(mean_trip_length = mean(trip_duration))
sel_may20 <- may20 %>%
  clean_names() %>%
  group_by(member_casual) %>%
  summarise(mean_trip_length = mean(trip_duration))
sel_juni20 <- juni20 %>%
  clean_names() %>%
  group_by(member_casual) %>%
  summarise(mean_trip_length = mean(trip_duration))
sel_jully20 <- jully20 %>%
  clean_names() %>%
  group_by(member_casual) %>%
  summarise(mean_trip_length = mean(trip_duration))
sel_august20 <- august20 %>%
  clean_names() %>%
  group_by(member_casual) %>%
  summarise(mean_trip_length = mean(trip_duration))
sel_september20 <- september20 %>%
  clean_names() %>%
  group_by(member_casual) %>%
  summarise(mean_trip_length = mean(trip_duration))
sel_october20 <- october20 %>%
  clean_names() %>%
  group_by(member_casual) %>%
  summarise(mean_trip_length = mean(trip_duration))
sel_november20 <- november20 %>%
  clean_names() %>%
  group_by(member_casual) %>%
  summarise(mean_trip_length = mean(trip_duration))
sel_december20 <- december20 %>%
  clean_names() %>%
  group_by(member_casual) %>%
  summarise(mean_trip_length = mean(trip_duration))

#monthly mean trips summary
mean_month_trips20 <-
  bind_rows(
    sel_april20,
    sel_may20,
    sel_juni20,
    sel_jully20,
    sel_august20,
    sel_september20,
    sel_october20,
    sel_november20,
    sel_december20
  )

months <-
  c(
    "April",
    "April",
    "May",
    "May",
    "Juni",
    "Juni",
    "Jully",
    "Jully",
    "August",
    "August",
    "September",
    "September",
    "October",
    "October",
    "November",
    "November",
    "December",
    "December"
  )


mean_month_trips20 <- mean_month_trips20 %>%
  mutate(mean_month_trips20, "months" = months) %>%
  group_by(months, member_casual) %>%
  summarise(mean_trip_length)
mean_trips20 <-
  spread(mean_month_trips20, member_casual, mean_trip_length)
head(mean_trips20)

#===============================================================================
mindate <- "april 2020"
maxdate <- "december 2020"
ggplot(data = mean_month_trips20) +
  geom_col(mapping=aes(x=months, y=mean_trip_length, fill=member_casual),position = position_dodge())+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title="Comparison of trip duration by rider type",
       caption=paste0("Data from: ", mindate, " to ", maxdate),
       x="Months",
       y="Trip length (min)",
       fill="Rider's status")

#===============================================================================
#                          BIND ALL MONTHS
#===============================================================================
all_trips20 <-
  bind_rows(
    april20,
    may20,
    juni20,
    jully20,
    august20,
    september20,
    october20,
    november20,
    december20
  )
#check for NAs
colSums(is.na(all_trips20))
# I am interested here for start_station_names ant that's ok

# Let's see histogram, how many trips there were each week day
#===============================================================================
#                           Calculate the mode of day_of_week
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
weekly_usage <- all_trips20 %>% drop_na() %>% group_by(member_casual,trip_wday) %>% summarize(count_use = n())
ggplot(data = all_trips20) +
  geom_bar(mapping=aes(x=trip_wday, fill = member_casual),position=position_dodge())+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title="Mode of day of week",x="Day od the week",y="Trips count",fill="Rider's status")
# calculate populatrity
popularity_of_stations <- all_trips20 %>% drop_na() %>% group_by(member_casual,start_station_name) %>% summarize(count_use = n()) %>% 
  arrange(desc(count_use)) %>% 
  print(n = 6)
# The most popular between member riders are:
#   1. Clark St & Elm St
#   2. Broadway & Barry Ave
#   3. St. Clair St & Erie St
# The most popular between casual riders are:
#   1. Streeter Dr & Grand Ave
#   2. Lake Shore Dr & Monroe St
#   3. Millenium Park

#
ggplot(data = all_trips20) +
  geom_bar(mapping=aes(x=member_casual, fill=rideable_type),position = position_dodge2())+
  labs(title="Comparison of trips count by rider type",
       x="Rider status",
       y="Count of trips")



