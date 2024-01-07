#Main models
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
