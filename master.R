#.libPaths(c("H:/Documents/R/win-library/4.1","C:/Program Files/R/R-4.2.2/library"))

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
library(scales)
#library packages
library(ggpubr)
library(egg)
library(gridExtra)
library(patchwork)


library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("count", "dplyr")
conflict_prefer("multinom", "nnet")
conflict_prefer("ggarrange", "egg")


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
source("R/functions.R")

#graph parameters
lwdline <- 1.2
size_axis <- 12
size_axis_title <- 14
size_legend <- 10
size_legend_title <- 12
axis.title.x.position <- element_text(margin = margin(t =12))

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
render("R/GAM_Summary_all_var_different_outcomes.Rmd", output_file = paste0("../output/",today(),"GAM_summary_all_var_different_outcomes.html"))

### Additional GAM: stratification, interaction, etc
  ## BW outcome
  render("R/stratified_models/BW_GAM_stratified_by_maternal_nationality_SEP.Rmd", output_file = paste0("../../output/stratification/",today(),"BW_GAM_stratif_by_mat_nationality_SEP.html"))
  render("R/stratified_models/BW_GAM_stratified_by_SSEP_Category.Rmd", output_file = paste0("../../output/stratification/",today(),"BW_GAM_stratif_by_SSEP_Category.html"))
  render("R/stratified_models/BW_GAM_stratified_by_language_region.Rmd", output_file = paste0("../../output/stratification/",today(),"BW_GAM_stratif_by_language_region.html"))
  render("R/stratified_models/BW_GAM_interaction_SSEP_X_language_region.Rmd", output_file = paste0("../../output/stratification/",today(),"BW_GAM_interaction_SSEP_X_language_reg.html"))
  render("R/GAM_zoom_in_heatwaves.Rmd", output_file = paste0("../output/",today(),"GAM_zoom_in_heatwaves.html"))

  ## PTB outcome
  render("R/stratified_models/PTB_GAM_stratified_by_SSEP_Category.Rmd", output_file = paste0("../../output/stratification/",today(),"PTB_GAM_stratif_by_SSEP_Category.html"))
  render("R/stratified_models/PTB_GAM_stratified_by_maternal_nationality.Rmd", output_file = paste0("../../output/stratification/",today(),"PTB_GAM_stratif_by_maternal_nationality.html"))
  render("R/stratified_models/PTB_GAM_stratified_by_Language_Region.Rmd", output_file = paste0("../../output/stratification/",today(),"PTB_GAM_stratif_by_Language_Region.html"))
  render("R/stratified_models/PTB_GAM_stratified_by_maternal_age.Rmd", output_file = paste0("../../output/stratification/",today(),"PTB_GAM_stratified_by_mat_age.html"))
  render("R/stratified_models/Basel_PTB_stratified_by_maternal_age.Rmd", output_file = paste0("../../output/stratification/",today(),"PTB_GAM_Basel_stratified_by_mat_age.html"))
  
  ## SB outcome
  render("R/stratified_models/SB_GAM_stratified_by_SSEP_Category.Rmd", output_file = paste0("../../output/stratification/",today(),"SB_GAM_stratif_by_SSEP_Category.html"))
  render("R/stratified_models/SB_GAM_stratified_by_Language_Region.Rmd", output_file = paste0("../../output/stratification/",today(),"SB_GAM_stratif_by_Language_Region.html"))
  render("R/stratified_models/SB_GAM_stratified_by_Maternal_Nationality.Rmd", output_file = paste0("../../output/stratification/",today(),"SB_GAM_stratif_by_Maternal_Nationality.html"))
  
  ## sensitivity analysis: primiparous women only
  render("R/primiparous_women_BW_PTB_stratification.Rmd", output_file = paste0("../output/primiparous_women/",today(),"BW_PTB_whole_models_and_stratif_by_Language_SEP_nationality_primiparous.html"))
  
  ## trimester effect
  render("R/trimester_effect_crises/COVID_trimester_effect.Rmd", output_file = paste0("../../output/trimester_effect_crisis",today(),"COVID_trimester_effect.html"))
  render("R/trimester_effect_crises/great_recession_trimester_effect.Rmd", output_file = paste0("../../output/trimester_effect_crisis",today(),"great_recession_trimester_effect.html"))
  render("R/trimester_effect_crises/heatwave_trimester_effect.Rmd", output_file = paste0("../../output/trimester_effect_crisis",today(),"heatwave_trimester_effect.html"))
  source("R/trimester_effect_crises/table_trimester_effect.R")
  
## Tables and graphs
render("R/whole_tb_charact_population.Rmd", output_file = paste0("../output/",today(),"whole_tb_charact_population.html"))
render("R/graphs/graphics.Rmd", output_file = paste0("../../output/graphs/",today(),"graphics.html"))
source("R/whole_tb_charact_population_inverting.R")
render("R/stratified_models/graphs/BW_GAM_graphs_stratification_nationality_language_SEP.Rmd", output_file = paste0("../../../output/stratification/graphs/",today(),"BW_GAM_stratification_nationality_language_SEP.html"))
render("R/stratified_models/graphs/graphs_without_lines_BW_GAM_graphs_stratification_nationality_language_SEP.Rmd", output_file = paste0("../../../output/stratification/graphs/",today(),"graphs_without_lines_BW_GAM_stratification_nationality_language_SEP.html"))
render("R/stratified_models/graphs/PTB_GAM_graphs_stratification_nationality_language_SEP.Rmd", output_file = paste0("../../../output/stratification/graphs/",today(),"PTB_GAM_stratification_nationality_language_SEP.html"))
render("R/stratified_models/graphs/graphs_without_lines_PTB_GAM_graphs_stratification_nationality_language_SEP.Rmd", output_file = paste0("../../../output/stratification/graphs/",today(),"graphs_without_lines_PTB_GAM_stratification_nationality_language_SEP.html"))
render("R/stratified_models/graphs/SB_GAM_graphs_stratification_nationality_language_SEP.Rmd", output_file = paste0("../../../output/stratification/graphs/",today(),"SB_GAM_stratification_nationality_language_SEP.html"))
