.libPaths(c("H:/Documents/R/win-library/4.1","C:/Program Files/R/R-4.1.2/library"))

#packages

library(here)
library(dplyr)
library(lubridate)
library(rmarkdown)
library(ggplot2)
library(mgcv)


#  data from 2007 only
source("R/bevn_from_2007.R")

bevn <- read.csv2(here("data", "bevn2007.csv"), sep = ",")
#ecological variables data
anth <- read.csv2(here("data", "Anthropo.csv"))
gem <- read.csv2(here("data", "Gemeinde.csv"))


#loading R codes
source("R/merging.R")
source("R/data_preparation.R")
source("R/Inclusions.R")


#render Rmd files in html
render("R/descript_and_univ_analysis.Rmd", output_file = paste0("../output/",today(),"descript_and_univ_analysis.html"))
