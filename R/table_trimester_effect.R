# Table for the trimester effect of all crises: COVID, heatwaves, Great Recession
# COVID
## BW
my.ci.covid_1st_trim
my.ci.covid_2nd_trim
my.ci.covid_3rd_trim
## PTB
my_ci_PTB_1st_trim_covid
my_ci_PTB_2nd_trim_covid
my_ci_PTB_3rd_trim_covid
## SB
my_ci_SB_1st_trim_covid  
my_ci_SB_2nd_trim_covid
my_ci_SB_3rd_trim_covid

first_trim_COVID_BW_PTB_SB <- cbind(my.ci.covid_1st_trim, my_ci_PTB_1st_trim_covid, my_ci_SB_1st_trim_covid)
sec_trim_COVID_BW_PTB_SB <- cbind(my.ci.covid_2nd_trim, my_ci_PTB_2nd_trim_covid, my_ci_SB_2nd_trim_covid)
third_trim_COVID_BW_PTB_SB <- cbind(my.ci.covid_3rd_trim, my_ci_PTB_3rd_trim_covid, my_ci_SB_3rd_trim_covid)
COVID_trim_effect <- rbind(first_trim_COVID_BW_PTB_SB, sec_trim_COVID_BW_PTB_SB, third_trim_COVID_BW_PTB_SB)
COVID_trim_effect

# Heatwave
## BW 
my_ci_HW_BW_1st_trim
my_ci_HW_BW_2nd_trim
my_ci_HW_BW_3rd_trim
## PTB
my_ci_PTB_HW_1st_trim
my_ci_PTB_HW_2nd_trim
my_ci_PTB_HW_3rd_trim
## SB
my_ci_SB_HW_1st_trim
my_ci_SB_HW_2nd_trim
my_ci_SB_HW_3rd_trim

first_trim_HW_BW_PTB_SB <- cbind(my_ci_HW_BW_1st_trim, my_ci_PTB_HW_1st_trim, my_ci_SB_HW_1st_trim)
sec_trim_HW_BW_PTB_SB <- cbind(my_ci_HW_BW_2nd_trim, my_ci_PTB_HW_2nd_trim, my_ci_SB_HW_2nd_trim)
third_trim_HW_BW_PTB_SB <- cbind(my_ci_HW_BW_3rd_trim, my_ci_PTB_HW_3rd_trim, my_ci_SB_HW_3rd_trim)
heatwave_trim_effect <- rbind(first_trim_HW_BW_PTB_SB, sec_trim_HW_BW_PTB_SB, third_trim_HW_BW_PTB_SB)
heatwave_trim_effect
# Great Recession
## BW
my_ci_GR_BW_1st_trim
my_ci_GR_BW_2nd_trim
my_ci_GR_BW_3rd_trim
## PTB
my_ci_PTB_GR_1st_trim
my_ci_PTB_GR_2nd_trim
my_ci_PTB_GR_3rd_trim
## SB
my_ci_SB_GR_1st_trim
my_ci_SB_GR_2nd_trim
my_ci_SB_GR_3rd_trim

# first_trim_GR_BW_PTB_SB <- cbind(my_ci_GR_BW_1st_trim, my_ci_PTB_GR_1st_trim, my_ci_SB_GR_1st_trim)
# sec_trim_GR_BW_PTB_SB <- cbind(my_ci_GR_BW_2nd_trim, my_ci_PTB_GR_2nd_trim, my_ci_SB_GR_2nd_trim)
# third_trim_GR_BW_PTB_SB <- cbind(my_ci_GR_BW_3rd_trim, my_ci_PTB_GR_3rd_trim, my_ci_SB_GR_3rd_trim)
# GR_trim_effect <- rbind(first_trim_GR_BW_PTB_SB, sec_trim_GR_BW_PTB_SB, third_trim_GR_BW_PTB_SB)
# GR_trim_effect

# whole_tb_crises_trim_effect <- rbind(heatwave_trim_effect, GR_trim_effect, COVID_trim_effect)


## BW all crises, all trimesters
BW_all_trim_all_crises <- rbind(my.ci.covid_1st_trim, my.ci.covid_2nd_trim, my.ci.covid_3rd_trim,
                                my_ci_HW_BW_1st_trim, my_ci_HW_BW_2nd_trim, my_ci_HW_BW_3rd_trim,
                                my_ci_GR_BW_1st_trim, my_ci_GR_BW_2nd_trim, my_ci_GR_BW_3rd_trim)

BW_all_trim_all_crises['Crisis'] <- c("Heatwave", " ", " ",
                      "Great Recession", " ", " ", 
                      "COVID-19", " ", " ")
BW_all_trim_all_crises['Trimester'] <- c("First", "Second", "Third", 
                       "First", "Second", "Third",
                       "First", "Second", "Third")
BW_all_trim_all_crises<-BW_all_trim_all_crises[, c('Crisis', "Trimester", "beta", "lci","uci", "d")]

BW_all_trim_all_crises_gt <- BW_all_trim_all_crises %>%
  gt()%>%
  tab_header(title="Trimester effect of each crisis on birthweight") %>%
  tab_spanner(label = "95% CI (g)", columns = c(lci, uci)) 
BW_all_trim_all_crises_gt

BW_all_trim_all_crises_gt_html <- BW_all_trim_all_crises_gt%>%
  gtsave(here("output/tables_paper", "BW_all_trim_all_crises_gt.html"))

BW_all_trim_all_crises_gt_docx <- BW_all_trim_all_crises_gt%>%
  gtsave(here("output/tables_paper", "BW_all_trim_all_crises_gt.docx"))

## PTB all crises, all trimesters
PTB_all_trim_all_crises <- rbind(my_ci_PTB_1st_trim_covid, my_ci_PTB_2nd_trim_covid, my_ci_PTB_3rd_trim_covid, 
                                 my_ci_PTB_HW_1st_trim, my_ci_PTB_HW_2nd_trim, my_ci_PTB_HW_3rd_trim, 
                                 my_ci_PTB_GR_1st_trim, my_ci_PTB_GR_2nd_trim, my_ci_PTB_GR_3rd_trim)
PTB_all_trim_all_crises['Crisis'] <- c("Heatwave", " ", " ",
                                      "Great Recession", " ", " ", 
                                      "COVID-19", " ", " ")
PTB_all_trim_all_crises['Trimester'] <- c("First", "Second", "Third", 
                                         "First", "Second", "Third",
                                         "First", "Second", "Third")
PTB_all_trim_all_crises<-PTB_all_trim_all_crises[, c('Crisis', "Trimester", "OR", "lci","uci", "d")]

PTB_all_trim_all_crises_gt <- PTB_all_trim_all_crises %>%
  gt()%>%
  tab_header(title="Trimester effect of each crisis on pretermbirth") %>%
  tab_spanner(label = "95% CI", columns = c(lci, uci)) 
PTB_all_trim_all_crises_gt

PTB_all_trim_all_crises_gt_html <- PTB_all_trim_all_crises_gt%>%
  gtsave(here("output/tables_paper", "PTB_all_trim_all_crises_gt.html"))

PTB_all_trim_all_crises_gt_docx <- PTB_all_trim_all_crises_gt%>%
  gtsave(here("output/tables_paper", "PTB_all_trim_all_crises_gt.docx"))

## SB all crises, all trimesters
SB_all_trim_all_crises <- rbind(my_ci_SB_1st_trim_covid, my_ci_SB_2nd_trim_covid, my_ci_SB_3rd_trim_covid,
                                my_ci_SB_HW_1st_trim, my_ci_SB_HW_2nd_trim, my_ci_SB_HW_3rd_trim,
                                my_ci_SB_GR_1st_trim, my_ci_SB_GR_2nd_trim, my_ci_SB_GR_3rd_trim)
SB_all_trim_all_crises['Crisis'] <- c("Heatwave", " ", " ",
                                       "Great Recession", " ", " ", 
                                       "COVID-19", " ", " ")
SB_all_trim_all_crises['Trimester'] <- c("First", "Second", "Third", 
                                          "First", "Second", "Third",
                                          "First", "Second", "Third")
SB_all_trim_all_crises<-SB_all_trim_all_crises[, c('Crisis', "Trimester", "OR", "lci","uci", "d")]

SB_all_trim_all_crises_gt <- SB_all_trim_all_crises %>%
  gt()%>%
  tab_header(title="Trimester effect of each crisis on stillbirth") %>%
  tab_spanner(label = "95% CI", columns = c(lci, uci)) 
SB_all_trim_all_crises_gt

SB_all_trim_all_crises_gt_html <- SB_all_trim_all_crises_gt%>%
  gtsave(here("output/tables_paper", "SB_all_trim_all_crises_gt.html"))

SB_all_trim_all_crises_gt_docx <- SB_all_trim_all_crises_gt%>%
  gtsave(here("output/tables_paper", "SB_all_trim_all_crises_gt.docx"))
