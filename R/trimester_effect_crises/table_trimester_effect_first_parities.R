##### October 2023, with file TRIMESTER_different_outcomes_FLU
## BW all crises, first and last trimesters UNADJUSTED FOR GA
BW_trim_all_crises_primip <- rbind(my.ci_BW_1st_trim_primip,my.ci_BW_last_trim_primip)

BW_trim_all_crises_primip['Crisis'] <- c("Heatwave", 
                                   "Great Recession",
                                   "COVID-19",
                                    "Heatwave", 
                                    "Great Recession",
                                    "COVID-19")
BW_trim_all_crises_primip['Trimester'] <- c("First", " ", " ", 
                                     "Last", " ", " ")
BW_trim_all_crises_primip<-BW_trim_all_crises_primip[, c("Trimester",'Crisis',"beta", "lci","uci", "d")]

BW_trim_all_crises_primip <- BW_trim_all_crises_primip %>%
  gt()%>%
  tab_header(title="Trimester effect of each crisis on birthweight") %>%
  tab_spanner(label = "95% CI (g)", columns = c(lci, uci))
BW_trim_all_crises_primip

BW_trim_all_crises_primip%>%
  gtsave(here("output/tables_paper/suppl_material", "BW_trim_all_crises_gt_primip.html"))

BW_trim_all_crises_primip%>%
  gtsave(here("output/tables_paper/suppl_material", "BW_trim_all_crises_gt_primip.docx"))


## PTB all crises, first and last trimesters UNADJUSTED FOR GA
PTB_trim_all_crises_primip <- rbind(my.ci_PTB_1st_trim_primip,my.ci_PTB_last_trim_primip)

PTB_trim_all_crises_primip['Crisis'] <- c("Heatwave", 
                                  "Great Recession",
                                  "COVID-19",
                                  "Heatwave", 
                                  "Great Recession",
                                  "COVID-19")
PTB_trim_all_crises_primip['Trimester'] <- c("First", " ", " ", 
                                     "Last", " ", " ")
PTB_trim_all_crises_primip<-PTB_trim_all_crises_primip[, c("Trimester",'Crisis',"OR", "lci","uci", "d")]

PTB_trim_all_crises_primip <- PTB_trim_all_crises_primip %>%
  gt()%>%
  tab_header(title="Trimester effect of each crisis on preterm birth") %>%
  tab_spanner(label = "95% CI", columns = c(lci, uci))  %>%
  tab_style(
    style = list(      cell_fill(color = "#E0E0E0")),
    locations = cells_body(rows=c(2,3,5), columns=c(2:6)))
PTB_trim_all_crises_primip

PTB_trim_all_crises_primip%>%
  gtsave(here("output/tables_paper/suppl_material", "PTB_trim_all_crises_gt_primip.html"))

PTB_trim_all_crises_primip%>%
  gtsave(here("output/tables_paper/suppl_material", "PTB_trim_all_crises_gt_primip.docx"))
