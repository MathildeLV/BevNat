##### October 2023, with file TRIMESTER_different_outcomes_FLU
## BW all crises, first and last trimesters UNADJUSTED FOR GA
BW_trim_all_crises_flu <- rbind(my.ci_BW_1st_trim_flu,my.ci_BW_last_trim_flu)

BW_trim_all_crises_flu['Crisis'] <- c("Heatwave", 
                                   "Flu",
                                   "COVID-19",
                                    "Heatwave", 
                                    "Flu",
                                    "COVID-19")
BW_trim_all_crises_flu['Trimester'] <- c("First", " ", " ", 
                                     "Last", " ", " ")
BW_trim_all_crises_flu<-BW_trim_all_crises_flu[, c("Trimester",'Crisis',"beta", "lci","uci", "d")]

BW_trim_all_crises_flu <- BW_trim_all_crises_flu %>%
  gt()%>%
  tab_header(title="Trimester effect of each crisis on birthweight") %>%
  tab_spanner(label = "95% CI (g)", columns = c(lci, uci))%>%
  tab_style(
    style = list(
      cell_fill(color = "#eaeaea")),
    locations = cells_body(rows=c(3,4,6), columns=c(2:6)))
BW_trim_all_crises_flu

BW_trim_all_crises_flu%>%
  gtsave(here("output/tables_paper/suppl_material", "BW_trim_all_crises_gt_flu.html"))

BW_trim_all_crises_flu%>%
  gtsave(here("output/tables_paper/suppl_material", "BW_trim_all_crises_gt_flu.docx"))


## SB all crises, first and last trimesters UNADJUSTED FOR GA
SB_trim_all_crises_flu <- rbind(my.ci_SB_1st_trim_flu,my.ci_SB_last_trim_flu)

SB_trim_all_crises_flu['Crisis'] <- c("Heatwave", 
                                  "Flu",
                                  "COVID-19",
                                  "Heatwave", 
                                  "Flu",
                                  "COVID-19")
SB_trim_all_crises_flu['Trimester'] <- c("First", " ", " ", 
                                     "Last", " ", " ")
SB_trim_all_crises_flu<-SB_trim_all_crises_flu[, c("Trimester",'Crisis',"OR", "lci","uci", "d")]

SB_trim_all_crises_flu <- SB_trim_all_crises_flu %>%
  gt()%>%
  tab_header(title="Trimester effect of each crisis on stillbirth") %>%
  tab_spanner(label = "95% CI", columns = c(lci, uci))  %>%
  tab_style(
    style = list(
      cell_fill(color = "#d6d6d6")),
    locations = cells_body(rows=c(2,5), columns=c(2:6))) %>%
tab_style(
  style = list(
    cell_fill(color = "#c2c2c2")),
  locations = cells_body(rows=c(6), columns=c(2:6)))
SB_trim_all_crises_flu

SB_trim_all_crises_flu%>%
  gtsave(here("output/tables_paper/suppl_material", "SB_trim_all_crises_gt_flu.html"))

SB_trim_all_crises_flu%>%
  gtsave(here("output/tables_paper/suppl_material", "SB_trim_all_crises_gt_flu.docx"))


## PTB all crises, first and last trimesters UNADJUSTED FOR GA
PTB_trim_all_crises_flu <- rbind(my.ci_PTB_1st_trim_flu,my.ci_PTB_last_trim_flu)

PTB_trim_all_crises_flu['Crisis'] <- c("Heatwave", 
                                  "Flu",
                                  "COVID-19",
                                  "Heatwave", 
                                  "Flu",
                                  "COVID-19")
PTB_trim_all_crises_flu['Trimester'] <- c("First", " ", " ", 
                                     "Last", " ", " ")
PTB_trim_all_crises_flu<-PTB_trim_all_crises_flu[, c("Trimester",'Crisis',"OR", "lci","uci", "d")]

PTB_trim_all_crises_flu <- PTB_trim_all_crises_flu %>%
  gt()%>%
  tab_header(title="Trimester effect of each crisis on preterm birth") %>%
  tab_spanner(label = "95% CI", columns = c(lci, uci))  %>%
  tab_style(
    style = list(
      cell_fill(color = "#d6d6d6")),
    locations = cells_body(rows=c(1:3,5), columns=c(2:6)))
PTB_trim_all_crises_flu

PTB_trim_all_crises_flu%>%
  gtsave(here("output/tables_paper/suppl_material", "PTB_trim_all_crises_gt_flu.html"))

PTB_trim_all_crises_flu%>%
  gtsave(here("output/tables_paper/suppl_material", "PTB_trim_all_crises_gt_flu.docx"))
