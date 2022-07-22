.libPaths(c("H:/Documents/R/win-library/4.1","C:/Program Files/R/R-4.1.2/library"))

#packages

library(here)
library(dplyr)
library(lubridate)
library(rmarkdown)
library(ggplot2)


# original data
bevn <- read.csv2(here("data", "bevn2006.csv"), sep = ",")
#ecological variables data
anth <- read.csv2(here("data", "Anthropo.csv"))
gem <- read.csv2(here("data", "Gemeinde.csv"))


#loading R codes
source("R/merging.R")
source("R/data_preparation.R")
source("R/Inclusions.R")




render("R/simple_graphs_tables.Rmd", output_file = paste0("../output/",today(),"simple_graphs_tables.html"))
