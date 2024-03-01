# adding labels  ####
my_labels <- c(mat_age = "maternal age (years)", 
               parity_cat = "parity",
               civil_status3= "civil status",
               mean_ssep2= "mean SSEP",
               mean_Alt_mean2="mean altitude",
               BW="birth weight (g)", 
               Language="language region",
               Urban3="urbanity",
               mother_nationality_cat2="maternal nationality",
               sex="sex",
               GA_weeks="gestational age (weeks)",
               stillbirth="stillbirth",
               PTB="preterm birth (<37 weeks) ", LBW="low birth weight (<2'500g)"
)
bevn_eco_in6_graph <- bevn_eco_in6

bevn_eco_in6_graph <- bevn_eco_in6_graph%>%
  mutate(mother_nationality_cat2= case_when(is.na(mother_nationality_cat2) ~ "missing or Oceanian",
                                            TRUE ~as.character(mother_nationality_cat2)),
         Language=case_when(is.na(Language) ~"missing",
                            TRUE ~as.character(Language)),
         Urban3=case_when(is.na(Urban3) ~"missing",
                          TRUE ~as.character(Urban3)),
         parity_cat=case_when(is.na(parity_cat) ~"missing",
                              TRUE ~as.character(parity_cat)),
         across(c(mother_nationality_cat2, Language, Urban3,parity_cat), as.factor)) 


attr(bevn_eco_in6_graph$stillbirth, 'levels')<- c("livebirth", "stillbirth")
attr(bevn_eco_in6_graph$sex, 'levels')<- c("male", "female")
attr(bevn_eco_in6_graph$PTB, "levels")<- c('term', 'preterm')
attr(bevn_eco_in6_graph$LBW, "levels")<- c('normal BW', 'LBW')
attr(bevn_eco_in6_graph$parity_cat, "levels")<- c('1', '2', '3', '>3',"missing")
attr(bevn_eco_in6_graph$Urban3, "levels")<- c('rural', 'urban', 'missing')
attr(bevn_eco_in6_graph$civil_status3, "levels")<- c('married', 'single')


desired_order_nat <- c("Switzerland","Europe","Asia","Africa","Southern and Central America","Northern America","missing or Oceanian")  
desired_order_lang <- c("German or Romansh","French","Italian","missing")

bevn_eco_in6_graph <- bevn_eco_in6_graph %>%
  mutate(Language = factor(Language, levels = desired_order_lang),
         mother_nationality_cat2=factor(mother_nationality_cat2, levels=desired_order_nat))


label(bevn_eco_in6_graph) <- as.list(my_labels[match(names(bevn_eco_in6_graph), # Assign labels to data frame variables
                                                     
                                                     names(my_labels))])
label(bevn_eco_in6_graph)


## overall, simplified, all years together ####

# Neonatal charact ####
neonat_charc <- bevn_eco_in6_graph %>%
  select(BW,
         sex,
         GA_weeks,PTB, 
         stillbirth, LBW
  ) %>%  
  gtsummary::tbl_summary(
    statistic = list(
      c("BW", "GA_weeks") ~ "{mean} ({sd})",
      c("sex","PTB",
        "stillbirth", "LBW") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("sex","PTB",
        "stillbirth", "LBW") ~ 1,
      c("BW", "GA_weeks") ~ 1),
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**")%>%
  modify_caption("**Neonatal characteristics**")
neonat_charc

neonat_charc %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables_paper", "neonatal_characteristics_in6_tblsummary.html"))

neonat_charc %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables_paper", "neonatal_characteristics_in6_tblsummary.docx"))


# Maternal charact ####
maternal_charact <- bevn_eco_in6_graph %>%
  select(parity_cat, Urban3, Language, mother_nationality_cat2, civil_status3, 
         mat_age, mean_ssep2, mean_Alt_mean2
  ) %>%  
  gtsummary::tbl_summary(
    statistic = list(
      c("mat_age", "mean_ssep2", "mean_Alt_mean2") ~ "{mean} ({sd})",
      c("parity_cat","Urban3",
        "Language", "mother_nationality_cat2", "civil_status3") ~ "{n} ({p}%)"
    ),
    digits = list(
      c("mat_age", "mean_ssep2", "mean_Alt_mean2", "Language", "mother_nationality_cat2", "civil_status3") ~ 1),
    missing_text = "missing"
  )%>%
  gtsummary::modify_header(label ~ "**Variable**")%>%
  modify_caption("**Maternal characteristics**")
maternal_charact

maternal_charact %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables_paper", "maternal_characteristics_in6_tblsummary.html"))

maternal_charact %>%
  gtsummary::as_gt() %>%
  gtsave(here("output/tables_paper", "maternal_characteristics_in6_tblsummary.docx"))
