#Use only data from 2006 (includes GA)
library(here)
library(dplyr)
#load data 
bevn <- read.csv2(here("data", "birth_iem.csv"))


bevn2006 <- bevn %>%
  filter(Ereignisjahr>2005)
table(bevn1$Kind..Gestationsalter.in.Tagen, useNA="always")

write.csv(bevn2006, here("data", "bevn2006.csv"))