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




## BW all crises, all trimesters, ADJUSTED FOR GA
BW_all_trim_all_crises <- rbind(my_ci_HW_BW_1st_trim, my_ci_HW_BW_2nd_trim, my_ci_HW_BW_3rd_trim,
                                my_ci_GR_BW_1st_trim, my_ci_GR_BW_2nd_trim, my_ci_GR_BW_3rd_trim,
                                my.ci.covid_1st_trim, my.ci.covid_2nd_trim, my.ci.covid_3rd_trim
                                )

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

##############

## BW all crises, all trimesters UNADJUSTED FOR GA
BW_all_trim_all_crises_unadj_GA <- rbind(my_ci_HW_BW_1st_trim_unadj_GA, my_ci_HW_BW_2nd_trim_unadj_GA, my_ci_HW_BW_3rd_trim_unadj_GA,
                                         my_ci_GR_BW_1st_trim_unadj_GA, my_ci_GR_BW_2nd_trim_unadj_GA, my_ci_GR_BW_3rd_trim_unadj_GA,
                                         my.ci.covid_1st_trim_unadj_GA, my.ci.covid_2nd_trim_unadj_GA, my.ci.covid_3rd_trim_unadj_GA
                                )

BW_all_trim_all_crises_unadj_GA['Crisis'] <- c("Heatwave", " ", " ",
                                      "Great Recession", " ", " ", 
                                      "COVID-19", " ", " ")
BW_all_trim_all_crises_unadj_GA['Trimester'] <- c("First", "Second", "Third", 
                                         "First", "Second", "Third",
                                         "First", "Second", "Third")
BW_all_trim_all_crises_unadj_GA<-BW_all_trim_all_crises_unadj_GA[, c('Crisis', "Trimester", "beta", "lci","uci", "d")]

BW_all_trim_all_crises_gt_unadj_GA <- BW_all_trim_all_crises_unadj_GA %>%
  gt()%>%
  tab_header(title="Trimester effect of each crisis on birthweight") %>%
  tab_spanner(label = "95% CI (g)", columns = c(lci, uci)) 
BW_all_trim_all_crises_gt_unadj_GA

BW_all_trim_all_crises_gt_html_unadj_GA <- BW_all_trim_all_crises_gt_unadj_GA%>%
  gtsave(here("output/tables_paper", "BW_all_trim_all_crises_gt_unadj_GA.html"))

BW_all_trim_all_crises_gt_docx <- BW_all_trim_all_crises_gt%>%
  gtsave(here("output/tables_paper", "BW_all_trim_all_crises_gt.docx"))

####################



# PTB all crises, all trimesters UNADJUSTED FOR BIRTHWEIGHT
my_ci_PTB_1st_trim_covid_unadj_BW <- read.csv2(here("output/models_trimester", "my_ci_PTB_covid_1st_trim_unadj_BW.csv"))
my_ci_PTB_2nd_trim_covid_unadj_BW <- read.csv2(here("output/models_trimester", "my_ci_PTB_covid_2nd_trim_unadj_BW.csv"))
my_ci_PTB_3rd_trim_covid_unadj_BW <- read.csv2(here("output/models_trimester", "my_ci_PTB_covid_3rd_trim_unadj_BW.csv"))
my_ci_PTB_HW_1st_trim_unadj_BW <- read.csv2(here("output/models_trimester", "my_ci_PTB_HW_1st_trim_unadj_BW.csv"))
my_ci_PTB_HW_2nd_trim_unadj_BW <- read.csv2(here("output/models_trimester", "my_ci_PTB_HW_2nd_trim_unadj_BW.csv"))
my_ci_PTB_HW_3rd_trim_unadj_BW <- read.csv2(here("output/models_trimester", "my_ci_PTB_HW_3rd_trim_unadj_BW.csv"))
my_ci_PTB_GR_1st_trim_unadj_BW <- read.csv2(here("output/models_trimester", "my_ci_PTB_GR_1st_trim_unadj_BW.csv"))
my_ci_PTB_GR_2nd_trim_unadj_BW <- read.csv2(here("output/models_trimester", "my_ci_PTB_GR_2nd_trim_unadj_BW.csv"))
my_ci_PTB_GR_3rd_trim_unadj_BW <- read.csv2(here("output/models_trimester", "my_ci_PTB_GR_3rd_trim_unadj_BW.csv"))


PTB_all_trim_all_crises_unadj_BW <- rbind(my_ci_PTB_HW_1st_trim_unadj_BW, my_ci_PTB_HW_2nd_trim_unadj_BW, my_ci_PTB_HW_3rd_trim_unadj_BW,
                                 my_ci_PTB_GR_1st_trim_unadj_BW, my_ci_PTB_GR_2nd_trim_unadj_BW, my_ci_PTB_GR_3rd_trim_unadj_BW ,
                                 my_ci_PTB_1st_trim_covid_unadj_BW, my_ci_PTB_2nd_trim_covid_unadj_BW, my_ci_PTB_3rd_trim_covid_unadj_BW 
)
PTB_all_trim_all_crises_unadj_BW['Crisis'] <- c("Heatwave", " ", " ",
                                       "Great Recession", " ", " ", 
                                       "COVID-19", " ", " ")
PTB_all_trim_all_crises_unadj_BW['Trimester'] <- c("First", "Second", "Third", 
                                          "First", "Second", "Third",
                                          "First", "Second", "Third")
PTB_all_trim_all_crises_unadj_BW<-PTB_all_trim_all_crises_unadj_BW[, c('Crisis', "Trimester", "OR", "lci","uci", "d")]

PTB_all_trim_all_crises_unadj_BW_gt <- PTB_all_trim_all_crises_unadj_BW %>%
  gt()%>%
  tab_header(title="Trimester effect of each crisis on pretermbirth") %>%
  tab_spanner(label = "95% CI", columns = c(lci, uci)) 
PTB_all_trim_all_crises_unadj_BW_gt

PTB_all_trim_all_crises_unadj_BW_gt_html <- PTB_all_trim_all_crises_unadj_BW_gt%>%
  gtsave(here("output/tables_paper", "PTB_all_trim_all_crises_unadj_BW_gt.html"))

PTB_all_trim_all_crises_unadj_BW_gt_docx <- PTB_all_trim_all_crises_unadj_BW_gt%>%
  gtsave(here("output/tables_paper", "PTB_all_trim_all_crises_unadj_BW_gt.docx"))





## SB all crises, all trimesters
SB_all_trim_all_crises <- rbind(my_ci_SB_HW_1st_trim, my_ci_SB_HW_2nd_trim, my_ci_SB_HW_3rd_trim,
                                my_ci_SB_GR_1st_trim, my_ci_SB_GR_2nd_trim, my_ci_SB_GR_3rd_trim,
                                my_ci_SB_1st_trim_covid, my_ci_SB_2nd_trim_covid, my_ci_SB_3rd_trim_covid
                                )
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





## SB all crises, all trimesters WITH MATERNAL NATIONALITY AS SWISS VS NON-SWISS
SB_all_trim_all_crises_mat_nat1 <- rbind(my_ci_SB_HW_1st_trim_nat1,
                                         my_ci_SB_HW_2nd_trim_nat1,
                                         my_ci_SB_HW_3rd_trim_nat1,
                                         my_ci_SB_GR_1st_trim_nat1,
                                         my_ci_SB_GR_2nd_trim_nat1,
                                         my_ci_SB_GR_3rd_trim_nat1,
                                         my_ci_SB_1st_trim_covid_nat1,
                                         my_ci_SB_2nd_trim_covid_nat1,
                                         my_ci_SB_3rd_trim_covid_nat1)
SB_all_trim_all_crises_mat_nat1['Crisis'] <- c("Heatwave", " ", " ",
                                      "Great Recession", " ", " ", 
                                      "COVID-19", " ", " ")
SB_all_trim_all_crises_mat_nat1['Trimester'] <- c("First", "Second", "Third", 
                                         "First", "Second", "Third",
                                         "First", "Second", "Third")
SB_all_trim_all_crises_mat_nat1<-SB_all_trim_all_crises_mat_nat1[, c('Crisis', "Trimester", "OR", "lci","uci", "d")]

SB_all_trim_all_crises_mat_nat1_gt <- SB_all_trim_all_crises_mat_nat1 %>%
  gt()%>%
  tab_header(title="Trimester effect of each crisis on stillbirth") %>%
  tab_spanner(label = "95% CI", columns = c(lci, uci)) 
SB_all_trim_all_crises_mat_nat1_gt

SB_all_trim_all_crises_mat_nat1_gt %>%
  gtsave(here("output/tables_paper", "SB_all_trim_all_crises_gt_mat_nat1.html"))

SB_all_trim_all_crises_mat_nat1_gt %>%
  gtsave(here("output/tables_paper", "SB_all_trim_all_crises_gt_mat_nat1.docx"))




## SB all crises, all trimesters 
# WITH MATERNAL NATIONALITY AS SWISS VS NON-SWISS
# and WITHOUT ADJUSTING for GESTATIONAL AGE
SB_all_trim_all_crises_mat_nat1_unadj_GA <- rbind(
  my_ci_SB_HW_1st_trim_nat1_unadj_GA,
  my_ci_SB_HW_2nd_trim_nat1_unadj_GA,
  my_ci_SB_HW_3rd_trim_nat1_unadj_GA,
  my_ci_SB_GR_1st_trim_nat1_unadj_GA,
  my_ci_SB_GR_2nd_trim_nat1_unadj_GA,
  my_ci_SB_GR_3rd_trim_nat1_unadj_GA,
  my_ci_SB_1st_trim_covid_nat1_unadj_GA,
  my_ci_SB_2nd_trim_covid_nat1_unadj_GA,
  my_ci_SB_3rd_trim_covid_nat1_unadj_GA)

SB_all_trim_all_crises_mat_nat1_unadj_GA['Crisis'] <- c("Heatwave", " ", " ",
                                               "Great Recession", " ", " ", 
                                               "COVID-19", " ", " ")
SB_all_trim_all_crises_mat_nat1_unadj_GA['Trimester'] <- c("First", "Second", "Third", 
                                                  "First", "Second", "Third",
                                                  "First", "Second", "Third")
SB_all_trim_all_crises_mat_nat1_unadj_GA<-SB_all_trim_all_crises_mat_nat1_unadj_GA[, c('Crisis', "Trimester", "OR", "lci","uci", "d")]

SB_all_trim_all_crises_mat_nat1_unadj_GA_gt <- SB_all_trim_all_crises_mat_nat1_unadj_GA %>%
  gt()%>%
  tab_header(title="Trimester effect of each crisis on stillbirth") %>%
  tab_spanner(label = "95% CI", columns = c(lci, uci)) 
SB_all_trim_all_crises_mat_nat1_unadj_GA_gt

SB_all_trim_all_crises_mat_nat1_unadj_GA_gt %>%
  gtsave(here("output/tables_paper", "SB_all_trim_all_crises_gt_mat_nat1_unadj_GA.html"))

SB_all_trim_all_crises_mat_nat1_unadj_GA_gt %>%
  gtsave(here("output/tables_paper", "SB_all_trim_all_crises_gt_mat_nat1_unadj_GA.docx"))




##### October 2023, with file TRIMESTER_different_outcomes_sept2023_3rd_trim_vs_not_epsoed
## BW all crises, first and last trimesters UNADJUSTED FOR GA
BW_trim_all_crises <- rbind(BW_1st_trim,BW_last_trim)

BW_trim_all_crises['Crisis'] <- c("Heatwave", 
                                   "Great Recession",
                                   "COVID-19",
                                    "Heatwave", 
                                    "Great Recession",
                                    "COVID-19")
BW_trim_all_crises['Trimester'] <- c("First", " ", " ", 
                                     "Last", " ", " ")
BW_trim_all_crises<-BW_trim_all_crises[, c("Trimester",'Crisis',"beta", "lci","uci", "d", "pvalue")]

BW_trim_all_crises <- BW_trim_all_crises %>%
  gt()%>%
  # tab_header(title="Table 5.A :Trimester effect of each crisis on birthweight") %>%
  tab_spanner(label = "95% CI (g)", columns = c(lci, uci)) %>%
  tab_style(
    style = list(
      cell_fill(color = "#eaeaea")),
    locations = cells_body(rows=c(3,4,6), columns=c(2:6)))
BW_trim_all_crises

BW_trim_all_crises%>%
  gtsave(here("output/tables_paper/pvalue_added_in_the_tables", "BW_trim_all_crises_gt.html"))

BW_trim_all_crises%>%
  gtsave(here("output/tables_paper/pvalue_added_in_the_tables", "BW_trim_all_crises_gt.docx"))


## SB all crises, first and last trimesters UNADJUSTED FOR GA
SB_trim_all_crises <- rbind(SB_1st_trim,SB_last_trim)

SB_trim_all_crises['Crisis'] <- c("Heatwave", 
                                  "Great Recession",
                                  "COVID-19",
                                  "Heatwave", 
                                  "Great Recession",
                                  "COVID-19")
SB_trim_all_crises['Trimester'] <- c("First", " ", " ", 
                                     "Last", " ", " ")
SB_trim_all_crises<-SB_trim_all_crises[, c("Trimester",'Crisis',"OR", "lci","uci", "d", "pvalue")]

SB_trim_all_crises <- SB_trim_all_crises %>%
  gt()%>%
  # tab_header(title="Table 5.C: Trimester effect of each crisis on stillbirth") %>%
  tab_spanner(label = "95% CI", columns = c(lci, uci))  %>%
  tab_style(
    style = list(
      cell_fill(color = "#d6d6d6")),
    locations = cells_body(rows=c(5), columns=c(2:6)))%>%
  tab_style(
    style = list(
      cell_fill(color = "#c2c2c2")),
    locations = cells_body(rows=c(6), columns=c(2:6)))
SB_trim_all_crises

SB_trim_all_crises%>%
  gtsave(here("output/tables_paper/pvalue_added_in_the_tables", "SB_trim_all_crises_gt.html"))

SB_trim_all_crises%>%
  gtsave(here("output/tables_paper/pvalue_added_in_the_tables", "SB_trim_all_crises_gt.docx"))


## PTB all crises, first and last trimesters UNADJUSTED FOR GA
PTB_trim_all_crises <- rbind(PTB_1st_trim,PTB_last_trim)

PTB_trim_all_crises['Crisis'] <- c("Heatwave", 
                                  "Great Recession",
                                  "COVID-19",
                                  "Heatwave", 
                                  "Great Recession",
                                  "COVID-19")
PTB_trim_all_crises['Trimester'] <- c("First", " ", " ", 
                                     "Last", " ", " ")
PTB_trim_all_crises<-PTB_trim_all_crises[, c("Trimester",'Crisis',"OR", "lci","uci", "d")]

PTB_trim_all_crises <- PTB_trim_all_crises %>%
  gt()%>%
  tab_header(title="Table 5.B: Trimester effect of each crisis on preterm birth") %>%
  tab_spanner(label = "95% CI", columns = c(lci, uci))  %>%
  tab_style(
    style = list(
      cell_fill(color = "#d6d6d6")),
    locations = cells_body(rows=c(1,3), columns=c(2:6)))
PTB_trim_all_crises

PTB_trim_all_crises%>%
  gtsave(here("output/tables_paper/pvalue_added_in_the_tables", "PTB_trim_all_crises_gt.html"))
PTB_trim_all_crises%>%
  gtsave(here("output/tables_paper/pvalue_added_in_the_tables", "PTB_trim_all_crises_gt.docx"))


# All outcomes together in one table
BW_trim_all_crises <- rbind(BW_1st_trim,BW_last_trim)
BW_trim_all_crises <- BW_trim_all_crises %>%
  rename("beta (g)"=beta,lci1=lci,
         uci1=uci,
         d1=d,p1=pvalue)
PTB_trim_all_crises <- rbind(PTB_1st_trim,PTB_last_trim)
PTB_trim_all_crises<-PTB_trim_all_crises %>%
  rename(OR2=OR, lci2=lci,
         uci2=uci,
         d2=d,
         p2=pvalue)
SB_trim_all_crises <- rbind(SB_1st_trim,SB_last_trim)
SB_trim_all_crises<- SB_trim_all_crises %>%
  rename(OR3=OR, lci3=lci,
         uci3=uci,
         d3=d,
         p3=pvalue)
all_outcomes_trim <- cbind(BW_trim_all_crises, PTB_trim_all_crises, SB_trim_all_crises)
df <- data.frame(Crisis = c("Heatwave", 
                            "Great Recession",
                            "COVID-19",
                            "Heatwave", 
                            "Great Recession",
                            "COVID-19"), Trimester=c("First", " ", " ", 
                                                     "Last", " ", " "))
all_outcomes_trim <-cbind(df,all_outcomes_trim)

all_outcomes_trim_gt <- all_outcomes_trim %>%
  gt() %>%
  # tab_header(title="Table 5: Trimester effect of each crisis") %>%
  tab_spanner(
    label = "Birthweight",
    columns = c("beta (g)", "lci1", "uci1", "d1", "p1")
  ) %>%
  tab_spanner(
    label = "Preterm birth",
    columns = c("OR2", "lci2", "lci2", "uci2","d2", "p2")
  ) %>%
  tab_spanner(
    label = "Stillbirth",
    columns = c("OR3", "lci3", "lci3", "uci3","d3","p3")
  ) %>%
  cols_label(
    lci1 = "lci",
    uci1 ="uci",
    d1="d",
    OR2="OR",
    lci2="lci",
    uci2="uci",
    d2="d",
    OR3="OR",
    lci3="lci",
    uci3="uci",
    d3="d"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#eaeaea")),
    locations = cells_body(rows=c(3,4,6), columns=c(3:6))) %>%
  tab_style(
    style = list(
      cell_fill(color = "#d6d6d6")),
    locations = cells_body(rows=c(1,3), columns=c(7:10))) %>%
  tab_style(
    style = list(
      cell_fill(color = "#d6d6d6")),
    locations = cells_body(rows=c(5), columns=c(11:14))) %>%
  tab_style(
    style = list(
      cell_fill(color = "#c2c2c2")),
    locations = cells_body(rows=c(6), columns=c(11:14)))
all_outcomes_trim_gt

all_outcomes_trim_gt%>%
  gtsave(here("output/tables_paper/pvalue_added_in_the_tables", "all_outcomes_trim_gt.html"))
all_outcomes_trim_gt%>%
  gtsave(here("output/tables_paper/pvalue_added_in_the_tables", "all_outcomes_trim_gt.docx"))
