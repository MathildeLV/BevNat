#Use only data from 2007 (includes GA)
library(here)
library(dplyr)
#load data 
bevn <- read.csv2(here("data", "birth_iem.csv"))

#GA missing from which year?
summary(bevn$Kind..Gestationsalter.in.Tagen)
bevn <- bevn %>%
  mutate(kind_ga_days_cat = cut (Kind..Gestationsalter.in.Tagen, breaks=c(112,200, 280, 330), include.lowest=TRUE))  
table(bevn$Ereignisjahr, bevn$kind_ga_days_cat, useNA="always")

bevn2007 <- bevn %>%
  filter(Ereignisjahr>2006)
table(bevn$Kind..Gestationsalter.in.Tagen, useNA="always")

write.csv(bevn2007, here("data", "bevn2007.csv"))






