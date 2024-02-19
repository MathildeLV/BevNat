## PTB all crises, first and last trimesters UNADJUSTED FOR GA
LBW_trim_all_crises <- rbind(LBW_1st_trim,LBW_last_trim)

LBW_trim_all_crises['Crisis'] <- c("Heatwave", 
                                       "Great Recession",
                                       "COVID-19",
                                       "Heatwave", 
                                       "Flu",
                                       "COVID-19")
LBW_trim_all_crises['Trimester'] <- c("First", " ", " ", 
                                          "Last", " ", " ")
LBW_trim_all_crises<-LBW_trim_all_crises[, c("Trimester",'Crisis',"OR", "lci","uci", "d")]

LBW_trim_all_crises <- LBW_trim_all_crises %>%
  gt()%>%
  tab_header(title="Trimester effect of each crisis on low birth weight") %>%
  tab_spanner(label = "95% CI", columns = c(lci, uci))
LBW_trim_all_crises

LBW_trim_all_crises%>%
  gtsave(here("output/tables_for_paper/suppl_material", "LBW_trim_all_crises_gt.html"))

LBW_trim_all_crises%>%
  gtsave(here("output/tables_for_paper/suppl_material", "LBW_trim_all_crises_gt.docx"))
