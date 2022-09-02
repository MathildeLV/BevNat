.libPaths(c("H:/Documents/R/win-library/4.1","C:/Program Files/R/R-4.2.1/library"))

#packages

library(here)
library(mgcv)
library(tidyverse)
library(lubridate)
library(rmarkdown)
library(ggplot2)
library(repr)
library(memisc)
library(tableone)
library(DT)
library(data.table)
library(gt)
library(glue)
library(wesanderson)  #nice colours ggplot
library(ggeffects)

library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("count", "dplyr")

#  data from 2007 only
# source("R/bevn_from_2007.R")

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
render("R/multivariate_analysis.Rmd", output_file = paste0("../output/",today(),"multivariate_analysis.html"))
render("R/whole_tb_charact_population.Rmd", output_file = paste0("../output/",today(),"whole_tb_charact_population.html"))
render("R/graphics.Rmd", output_file = paste0("../output/",today(),"graphics.html"))
