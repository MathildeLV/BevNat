.libPaths(c("H:/Documents/R/win-library/4.1","C:/Program Files/R/R-4.3.2/library"))

#packages
library(dplyr)
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
library(askpass)
library(ftExtra) # to add parameters for gt tables
library(glue)
library(ggeffects)
library(rmdformats)
library(oddsratio)
library(nnet)
library(network)
 # library(lpSolveAPI)
 library(RDS)
# library(quantreg)
library(scales)
library(ggpubr)
library(egg)
library(gridExtra)
library(patchwork)
library(effects)
library(broom) 
library(conflicted)
library(usdm) # for VIF multicollinearity factor
library(LaplacesDemon)
library("Hmisc") #for labeling variables
library(gtsummary) # for gt summary tables of models (gtsummary::tbl_regression(m))

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("count", "dplyr")
conflict_prefer("multinom", "nnet")
conflict_prefer("ggarrange", "egg")


#  data from 2007 only
bevn <- read.csv2(here("data", "bevn2007.csv"), sep = ",")
# add years 2021 and 2022
bevn_2021 <- read.csv2(here("data", "NAISS21.csv"))
bevn_2022 <- read.csv2(here("data", "NAISS22.2.csv"),  fileEncoding = 'UTF-8-BOM')

#ecological variables data
# anth <- read.csv2(here("data", "Anthropo.csv")) # to add MS Region number
gem <- read.csv2(here("data", "Gemeinde.csv")) # to add municipality number, SSEP, Language region, Urbanity, altitude

# Whole Swiss population, permanent resident or not, population 2007 to 2022
pop <- read.csv2(here("data", "permanent_and_non_permanent_population_2010_2022.csv"),  fileEncoding = 'UTF-8-BOM')

# covid hospitalization numbers per week
data.covid.org <- read.csv2(here("data", "COVID19Hosp_geoRegion_w.csv"))


#graph parameters
lwdline <- 1.2
size_axis <- 12
size_axis_title <- 14
size_legend <- 10
size_legend_title <- 12
axis.title.x.position <- element_text(margin = margin(t =12))

#loading R codes
source("R/functions.R")
source("R/merging.R") #to run separately
source("R/data_preparation.R") #to run separately
source("R/covid_monthly_exposure_hospitalisations.R")  # create variable for exposure to COVID (sum of hospitalisation cases during pregnancy)
source("R/dummyvariables_crisis.R") #variables for exposure to heatwave, great recession, and 2009 flu pandemic
source("R/Inclusions_fast.R")


# Render Rmd files in html
#Univ/descriptive and multiv analysis
# render("R/descript_and_univ_analysis.Rmd", output_file = paste0("../output/",today(),"descript_and_univ_analysis.html"))
# render("R/multivariate_analysis.Rmd", output_file = paste0("../output/",today(),"multivariate_models_lm_glm.html"))

# Methods: table 1 for exposure to the crises through time
render("R/crises_exposure_variables_simple_tb_and_association_with_birth_outcomes.Rmd", output_file = paste0("../output/",today(),"crises_exposure_variables_through time.html"))

## GAM models
### univariable models
render("R/GAM_models_BW_PTB_SB_univariable_with_time_only.Rmd", output_file = paste0("../output/",today(),"GAM_models_BW_PTB_SB_univariable_with_time_only.html"))
### multivariable models
render("R/GAM_main_multivariate_models.Rmd", output_file = paste0("../output/",today(),"GAM_main_multivariate_models.html"))
### stratified models
render("R/stratified_models/graphs_without_lines_BW_GAM_graphs_stratification_nationality_language_SEP_sex.Rmd", output_file = paste0("../output/stratification",today(),"BW_GAM_graphs_stratification_nationality_language_SEP_sex.html"))
render("R/stratified_models/graphs_without_lines_PTB_GAM_graphs_stratification_nationality_language_SEP_sex.Rmd", output_file = paste0("../output/stratification",today(),"PTV_GAM_graphs_stratification_nationality_language_SEP_sex.html"))
render("R/stratified_models/graphs_without_lines_SB_GAM_graphs_stratification_nationality_language_SEP_sex.Rmd", output_file = paste0("../output/stratification",today(),"SB_GAM_graphs_stratification_nationality_language_SEP_sex.html"))



### Additional GAMs: stratification, interaction, etc
  ## sensitivity analysis: primiparous women only
  render("R/GAM_BW_PTB_first_parities.Rmd", output_file = paste0("../output/",today(),"BW_PTB_primiparous.html"))
  ## sensitivity analysis: flu exposure instead of Great Recession exposure
  render("R/GAM_models_flu_pandemic.Rmd", output_file = paste0("../output/",today(),"GAM_models_flu_pandemic.html"))
  

## trimester effect
  render("R/trimester_effect_crises/COVID_trimester_effect.Rmd", output_file = paste0("../../output/trimester_effect_crisis",today(),"COVID_trimester_effect.html"))
  render("R/trimester_effect_crises/great_recession_trimester_effect.Rmd", output_file = paste0("../../output/trimester_effect_crisis",today(),"great_recession_trimester_effect.html"))
  render("R/trimester_effect_crises/heatwave_trimester_effect.Rmd", output_file = paste0("../../output/trimester_effect_crisis",today(),"heatwave_trimester_effect.html"))
  source("R/trimester_effect_crises/table_trimester_effect.R")
 
###Tables for describing charactertics of the studied populations
  # including birth rate per year 
  render("R/tables/whole_tb_charact_population.Rmd", output_file = paste0("../output/",today(),"whole_tb_charact_population.html"))
  source("R/tables/whole_tb_charact_population_inverting.R")
  
### Figures
  source("R/graphs/figures_papers.R")
  render("R/graphs/covariates_through_time_and_number_births_per_covariate.Rmd", output_file = paste0("../output/figures_paper",today(),"birthrate_through_time.html"))
  