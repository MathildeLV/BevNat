.libPaths(c("H:/Documents/R/win-library/4.1","C:/Program Files/R/R-4.2.2/library"))

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
library(ggeffects)
library(rmdformats)
library(oddsratio)
library(nnet)
library(network)
library(RDS)
library(quantreg)

library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("count", "dplyr")
conflict_prefer("multinom", "nnet")


#  data from 2007 only
# source("R/bevn_from_2007.R")

bevn <- read.csv2(here("data", "bevn2007.csv"), sep = ",")
bevn_2021 <- read.csv2(here("data", "NAISS21.csv"))
#ecological variables data
anth <- read.csv2(here("data", "Anthropo.csv"))
gem <- read.csv2(here("data", "Gemeinde.csv"))


#loading R codes
source("R/merging.R")
source("R/data_preparation.R") #to run separately
source("R/dummyvariables_crisis.R")
source("R/Inclusions.R")


# Render Rmd files in html
#Univ/descriptive and multiv analysis
render("R/descript_and_univ_analysis.Rmd", output_file = paste0("../output/",today(),"descript_and_univ_analysis.html"))
render("R/multivariate_analysis.Rmd", output_file = paste0("../output/",today(),"multivariate_models_lm_glm.html"))
## Gam models
render("R/gam_birthweight.Rmd", output_file = paste0("../output/",today(),"gam_birthweight.html"))
render("R/gam_stillbirth.Rmd", output_file = paste0("../output/",today(),"gam_stillbirth.html"))
render("R/gam_gest_age.Rmd", output_file = paste0("../output/",today(),"gam_gest_age.html"))
render("R/gam_LBW_normal_macro.Rmd", output_file = paste0("../output/",today(),"gam_LBW_normal_macro.html"))
render("R/GAM_summary.Rmd", output_file = paste0("../output/",today(),"GAM_summary.html"))
render("R/GAM_summary_meeting_20221111.Rmd", output_file = paste0("../output/",today(),"GAM_summary_meeting_20221111.html"))
render("R/GAM_stratified_by_maternal_nationality_SEP.Rmd", output_file = paste0("../output/",today(),"GAM_stratif_by_mat_nationality_SEP.html"))
render("R/GAM_stratified_by_SSEP_Category.Rmd", output_file = paste0("../output/",today(),"GAM_stratif_by_SSEP_Category.html"))
render("R/GAM_stratified_by_language_region.Rmd", output_file = paste0("../output/",today(),"GAM_stratif_by_language_region.html"))
render("R/GAM_interaction_SSEP_X_language_region.Rmd", output_file = paste0("../output/",today(),"GAM_interaction_SSEP_X_language_reg.html"))

## Tables and graphs
render("R/whole_tb_charact_population.Rmd", output_file = paste0("../output/",today(),"whole_tb_charact_population.html"))
render("R/graphics.Rmd", output_file = paste0("../output/",today(),"graphics.html"))
source("R/whole_tb_charact_population_inverting.R")
