#Run whole_tb_population.Rmd beforehand!!

# Maternal characteristics

## At inclusion : in
###v1: no grouped rows
formatteda_inv <- formatteda
mat_in_inv <- t(formatteda_inv)
rownames(mat_in_inv) <- colnames(formatteda_inv)
colnames(mat_in_inv) <- rownames(formatteda_inv)
mat_in_inv

#removing the first row that is useless, to have fir row (and col names) as birthyear
colnames(mat_in_inv) <- mat_in_inv[1,]
mat_in_inv <- mat_in_inv[-1, ]
mat_in_inv
mat_in_inv <- as_tibble(mat_in_inv)
mat_in_inv

# 
# mat_in_inv_v1 <- add_column(mat_in_inv, .before = 1,
#                    name = c("Parity: 1 (%)", "2 (%)", "3 (%)", "> 3(%)",
#                             "Urbanicity: Rural  (%)", "Urban  (%)",
#                             "Language region: German or Romansh  (%)", "French  (%)", "Italian  (%)",
#                             "Mother nationality (cat2): Switzerland  (%)", "Africa  (%)", "Asia  (%)", "Europe  (%)",
#                             "Northern America  (%)", "Southern and Central America  (%)", "Missing  or Oceania (%)",
#                              "Civil status: Married (%)", "Single (%)",
#                             "Maternal age (years): mean ± sd ", "Mean SSEP: mean ± sd",
#                             "Mean altitude (MASL): mean ± sd","n")
# )
# mat_in_inv_v1
# 
# mat_in_inv_v1 <- mat_in_inv_v1%>%
#    gt()%>%
#   tab_header(title="Maternal characteristics at inclusion")%>%
#    cols_label(name = "Birthyear")
# mat_in_inv_v1


### V2: grouped rows
mat_in_inv_v2 <- add_column(mat_in_inv, .before = 1,
                   name = c("1", "2", "3", ">3", "missing", 
                            "Rural", "Urban", "missing",
                            "German or Romansh", "French", "Italian", "missing",
                            "Switzerland", "Africa", "Asia", "Europe", "Northern America", "Southern and Central America", "Missing or Oceania", 
                            "Married", "Single",
                            "Maternal age (years): mean ± sd", "Mean SSEP: mean ± sd", "Mean altitude (MASL): mean ± sd", "n")
)


mat_in_inv_v2 <- mat_in_inv_v2 %>%
  gt() %>%
  tab_header(title=md("**Maternal characteristics of the eligible population**"), 
             subtitle = "n=1,517,571") %>%
  tab_row_group(label="Parity (%)",
    rows=1:5) %>%
  tab_row_group(label="Urbanicity (%)",
    rows=6:8) %>%
  tab_row_group(label="Language region (%)",
    rows=9:12) %>%
  tab_row_group(label="Nationality (%)",
    rows=13:19) %>%
  tab_row_group(label="Civil status (%)",
                rows=20:21) %>%
  tab_row_group(label="Continuous variables",
                rows=22:24) %>%
  row_group_order(groups=c("Parity (%)", "Urbanicity (%)", "Language region (%)", "Nationality (%)", "Civil status (%)", "Continuous variables")
                           )%>%
  cols_label(name = "Birthyear") %>%
  cols_width(name ~ px(140), everything() ~ px(50)) %>%
  tab_options(table.width = 0.1, data_row.padding = px(1),   table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  )
mat_in_inv_v2

mat_in_inv_v2 %>%
  gtsave(here("output/tables_paper", "maternal_characteristics_in.html")) 
mat_in_inv_v2 %>% 
  gtsave(here("output/tables_paper", "maternal_characteristics_in.docx"), encoding = "latin1")



## In6: dataset bevn_eco_in6, grouped rows
formattedc_inv <- formattedc
mat_in_inv_6 <- t(formattedc_inv)
rownames(mat_in_inv_6) <- colnames(formattedc_inv)
colnames(mat_in_inv_6) <- rownames(formattedc_inv)
mat_in_inv_6

#removing the first row that is useless, to have fir row (and col names) as birthyear
colnames(mat_in_inv_6) <- mat_in_inv_6[1,]
mat_in_inv_6 <- mat_in_inv_6[-1, ] 
mat_in_inv_6
mat_in_inv_6 <- as_tibble(mat_in_inv_6)
mat_in_inv_6

### V2: grouped rows
mat_in_inv_6 <- add_column(mat_in_inv_6, .before = 1,
                           name = c("1", "2", "3", ">3", "missing",
                                    "Rural", "Urban", "missing",
                                    "German or Romansh", "French", "Italian", "missing",
                                    "Switzerland", "Africa", "Asia", "Europe", "Northern America", "Southern and Central America", "Missing or Oceania", 
                                    "Married", "Single",
                                    "Maternal age (years): mean ± sd", "Mean SSEP: mean ± sd", "Mean altitude (MASL): mean ± sd", "n")
)

mat_in_inv_6

mat_in_inv_6 <- mat_in_inv_6 %>%
  gt() %>%
  tab_header(title=md("**Maternal characteristics of the analysed population (incl. stillbirths)**"), 
             subtitle = "n=1,274,449") %>%
  tab_row_group(label="Parity (%)",
                rows=1:5) %>%
  tab_row_group(label="Urbanicity (%)",
                rows=6:8) %>%
  tab_row_group(label="Language region (%)",
                rows=9:12) %>%
  tab_row_group(label="Nationality (%)",
                rows=13:19) %>%
  tab_row_group(label="Civil status (%)",
                rows=20:21) %>%
  tab_row_group(label="Continuous variables",
                rows=22:24) %>%
  row_group_order(groups=c("Parity (%)", "Urbanicity (%)", "Language region (%)", "Nationality (%)", "Civil status (%)", "Continuous variables")
  )%>%
  cols_label(name = "Birthyear") %>%
  cols_width(name ~ px(140), everything() ~ px(50)) %>%
  tab_options(table.width = 0.1, data_row.padding = px(1),   table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  )
mat_in_inv_6

mat_in_inv_6 %>%
  gtsave(here("output/tables_paper", "maternal_characteristics_in6.html"))
mat_in_inv_6 %>% 
  gtsave(here("output/tables_paper", "maternal_characteristics_in6.docx"))


## In7: dataset bevn_eco_in7, grouped rows
mat_in_inv_7 <- t(formattedb)
rownames(mat_in_inv_7) <- colnames(formattedb)
colnames(mat_in_inv_7) <- rownames(formattedb)
mat_in_inv_7

#removing the first row that is useless, to have fir row (and col names) as birthyear
colnames(mat_in_inv_7) <- mat_in_inv_7[1,]
mat_in_inv_7 <- mat_in_inv_7[-1, ] 
mat_in_inv_7
mat_in_inv_7 <- as_tibble(mat_in_inv_7)
mat_in_inv_7

mat_in_inv_7 <- add_column(mat_in_inv_7, .before = 1,
                           name = c("1", "2", "3", ">3", "missing",
                                    "Rural", "Urban", "missing",
                                    "German or Romansh", "French", "Italian", "missing",
                                    "Switzerland", "Africa", "Asia", "Europe", "Northern America", "Southern and Central America", "Missing or Oceania", 
                                    "Married", "Single",
                                    "Maternal age (years): mean ± sd", "Mean SSEP: mean ± sd", "Mean altitude (MASL): mean ± sd", "n")
)

mat_in_inv_7

mat_in_inv_7 <- mat_in_inv_7 %>%
  gt() %>%
  tab_header(title=md("**Maternal characteristics of the analysed population (excl. stillbirths)**"), 
             subtitle = "n=1,269,586") %>%
  tab_row_group(label="Parity (%)",
                rows=1:5) %>%
  tab_row_group(label="Urbanicity (%)",
                rows=6:8) %>%
  tab_row_group(label="Language region (%)",
                rows=9:12) %>%
  tab_row_group(label="Nationality (%)",
                rows=13:19) %>%
  tab_row_group(label="Civil status (%)",
                rows=20:21) %>%
  tab_row_group(label="Continuous variables",
                rows=22:24) %>%
  row_group_order(groups=c("Parity (%)", "Urbanicity (%)", "Language region (%)", "Nationality (%)", "Civil status (%)", "Continuous variables")
  )%>%
  cols_label(name = "Birthyear") %>%
  cols_width(name ~ px(140), everything() ~ px(50)) %>%
  tab_options(table.width = 0.1, data_row.padding = px(1),   table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  )
mat_in_inv_7

mat_in_inv_7 %>%
  gtsave(here("output/tables_paper", "maternal_characteristics_in7.html"))
mat_in_inv_7 %>%
  gtsave(here("output/tables_paper", "maternal_characteristics_in7.docx"))


# Neonatal characteristics
## At inclusion : in
neo_in_inv <- t(formatted0)
rownames(neo_in_inv) <- colnames(formatted0)
colnames(neo_in_inv) <- rownames(formatted0)
neo_in_inv

#removing the first row that is useless, to have fir row (and col names) as birthyear
colnames(neo_in_inv) <- neo_in_inv[1,]
neo_in_inv <- neo_in_inv[-1, ] 
neo_in_inv
neo_in_inv <- as_tibble(neo_in_inv)
neo_in_inv

neo_in_inv <- add_column(neo_in_inv, .before = 1,
                           name = c("n", "Birthweight (g), mean ± sd", "Gestational age (weeks), mean ± sd", 
                                    "Female (%)", "Male (%)", "Stillbirth (%)", "Preterm birth (%)", "Low birthweight (%)"
                                    )
)

neo_in_inv

 neo_in_inv %>%
  gt() %>%
  tab_header(title=md("**Neonatal characteristics of the eligible population**"),
             subtitle= "n=1,517,571") %>%
     cols_label(name = "Birthyear") %>%
   cols_width(name ~ px(120), everything() ~ px(50)) %>%
   tab_options(table.width = 0.1, data_row.padding = px(1), 
               heading.title.font.size = 14, 
               heading.subtitle.font.size = 14,
               table.font.size = 12,
               column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
               
   ) %>%
  gtsave(here("output/tables_paper", "neonatal_characteristics_in.html"))
  neo_in_inv
  
  neo_in_inv %>%
    gt()%>%
    gtsave(here("output/tables_paper", "neonatal_characteristics_in.docx"))

## In dataset 6
neo_in6_inv <- t(formatted2)
rownames(neo_in6_inv) <- colnames(formatted2)
colnames(neo_in6_inv) <- rownames(formatted2)
neo_in6_inv

#removing the first row that is useless, to have fir row (and col names) as birthyear
colnames(neo_in6_inv) <- neo_in6_inv[1,]
neo_in6_inv <- neo_in6_inv[-1, ] 
neo_in6_inv
neo_in6_inv <- as_tibble(neo_in6_inv)
neo_in6_inv

neo_in6_inv <- add_column(neo_in6_inv, .before = 1,
                         name = c("n", "Birthweight (g), mean ± sd", "Gestational age (weeks), mean ± sd", 
                                  "Female (%)", "Male (%)", "Stillbirth (%)", "Preterm birth (%)", "Low birthweight (%)")
)
neo_in6_inv


neo_in6_inv %>%
  gt() %>%
  tab_header(title=md("**Neonatal characteristics of the analysed population (incl. stillbirths)**"),
                      subtitle="n=1,274,449") %>%
  cols_label(name = "Birthyear") %>%
  cols_width(name ~ px(120), everything() ~ px(50)) %>%
  tab_options(table.width = 0.1, data_row.padding = px(1), 
              heading.title.font.size = 14, 
              heading.subtitle.font.size = 14,
              table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              
  ) %>%
  gtsave(here("output/tables_paper", "neonatal_characteristics_in6.html"))
neo_in6_inv

neo_in6_inv %>%
  gt()%>%
  gtsave(here("output/tables_paper", "neonatal_characteristics_in6.docx"))

## In dataset 7
neo_in7_inv <- t(formatted1)
rownames(neo_in7_inv) <- colnames(formatted1)
colnames(neo_in7_inv) <- rownames(formatted1)
neo_in7_inv

#removing the first row that is useless, to have fir row (and col names) as birthyear
colnames(neo_in7_inv) <- neo_in7_inv[1,]
neo_in7_inv <- neo_in7_inv[-1, ] 
neo_in7_inv
neo_in7_inv <- as_tibble(neo_in7_inv)
neo_in7_inv

neo_in7_inv <- add_column(neo_in7_inv, .before = 1,
                          name = c("n", "Birthweight (g), mean ± sd", "Gestational age (weeks), mean ± sd", 
                                   "Female (%)", "Male (%)", "Preterm birth (%)", "Low birthweight (%)")
)
neo_in7_inv


neo_in7_inv %>%
  gt() %>%
  tab_header(title=md("**Neonatal characteristics of the analysed population (excl. stillbirths)**"),
             subtitle = "n=1,269,586")%>%
  cols_label(name = "Birthyear") %>%
  cols_width(name ~ px(120), everything() ~ px(50)) %>%
  tab_options(table.width = 0.1, data_row.padding = px(1),   table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  ) %>%
gtsave(here("output/tables_paper", "neonatal_characteristics_in7.html"))
neo_in7_inv

neo_in7_inv %>%
  gt()%>%
  gtsave(here("output/tables_paper", "neonatal_characteristics_in7.docx"))


# Combined maternal and neonatal characteristics
# initial dataset bev_eco
formattedcomb0_inv <- formattedcomb0
combined_in0_inv <- t(formattedcomb0_inv)
rownames(combined_in0_inv) <- colnames(formattedcomb0_inv)
colnames(combined_in0_inv) <- rownames(formattedcomb0_inv)
combined_in0_inv

#removing the first row that is useless, to have fir row (and col names) as birthyear
colnames(combined_in0_inv) <- combined_in0_inv[1,]
combined_in0_inv <- combined_in0_inv[-1, ]
combined_in0_inv
combined_in0_inv <- as_tibble(combined_in0_inv)
combined_in0_inv


### V2: grouped rows
combined_in0_inva <- add_column(combined_in0_inv, .before = 1,
                            name = c("1", "2", "3", ">3", "missing", 
                                     "Rural", "Urban", "missing",
                                     "German or Romansh", "French", "Italian", "missing",
                                     "Switzerland", "Africa", "Asia", "Europe", "Northern America", "Southern and Central America", "Missing or Oceania", 
                                     "Married", "Single",
                                     "Maternal age (years): mean ± sd", "Mean SSEP: mean ± sd", "Mean altitude (MASL): mean ± sd", 
                                     "Birthweight (g), mean ± sd", "Gestational age (weeks), mean ± sd", 
                                     "Female (%)", "Male (%)", "Stillbirth (%)", "Preterm birth (%)", "Low birthweight (%)",
                                     "n")
)


combined_in0_inva <- combined_in0_inva %>%
  gt() %>%
  tab_header(title=md("**Maternal and neonatal characteristics of the eligible population**"), 
             subtitle = "n=1,517,571") %>%
  tab_row_group(label="Parity (%)",
                rows=1:5) %>%
  tab_row_group(label="Urbanicity (%)",
                rows=6:8) %>%
  tab_row_group(label="Language region (%)",
                rows=9:12) %>%
  tab_row_group(label="Maternal nationality (%)",
                rows=13:19) %>%
  tab_row_group(label="Civil status (%)",
                rows=20:21) %>%
  tab_row_group(label="Continuous variables",
                rows=22:24) %>%
  tab_row_group(label="Neonatal outcomes",
                rows=25:31) %>%
  row_group_order(groups=c("Parity (%)", "Urbanicity (%)", "Language region (%)", 
                           "Maternal nationality (%)", "Civil status (%)", "Continuous variables", "Neonatal outcomes")
  )%>%
  cols_label(name = "Birthyear") %>%
  cols_width(name ~ px(140), everything() ~ px(50)) %>%
  tab_options(table.width = 0.1, data_row.padding = px(1),   table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  )
combined_in0_inva

combined_in0_inva %>%
  gtsave(here("output/tables_paper", "maternal_neonatal_characteristics_in.html")) 
combined_in0_inva %>% 
  gtsave(here("output/tables_paper", "maternal_neonatal_characteristics_in.docx"), encoding = "latin1")


# Combined maternal and neonatal characteristics
# dataset bev_eco_in6 with stillbirth
formattedcomb6_inv <- formattedcomb6
combined_in6_inv <- t(formattedcomb6_inv)
rownames(combined_in6_inv) <- colnames(formattedcomb6_inv)
colnames(combined_in6_inv) <- rownames(formattedcomb6_inv)
combined_in6_inv

#removing the first row that is useless, to have fir row (and col names) as birthyear
colnames(combined_in6_inv) <- combined_in6_inv[1,]
combined_in6_inv <- combined_in6_inv[-1, ]
combined_in6_inv
combined_in6_inv <- as_tibble(combined_in6_inv)
combined_in6_inv

combined_in6_inva <- add_column(combined_in6_inv, .before = 1,
                                name = c("1", "2", "3", ">3", "missing", 
                                         "Rural", "Urban", "missing",
                                         "German or Romansh", "French", "Italian", "missing",
                                         "Switzerland", "Africa", "Asia", "Europe", "Northern America", "Southern and Central America", "Missing or Oceania", 
                                         "Married", "Single",
                                         "Maternal age (years): mean ± sd", "Mean SSEP: mean ± sd", "Mean altitude (MASL): mean ± sd", 
                                         "Birthweight (g), mean ± sd", "Gestational age (weeks), mean ± sd", 
                                         "Female (%)", "Male (%)", "Stillbirth (%)", "Preterm birth (%)", "Low birthweight (%)",
                                         "n")
)


combined_in6_inva <- combined_in6_inva %>%
  gt() %>%
  tab_header(title=md("**Maternal and neonatal characteristics of the analysed population (incl. stillbirths)**"), 
             subtitle = "n=1,274,449") %>%
  tab_row_group(label="Parity (%)",
                rows=1:5) %>%
  tab_row_group(label="Urbanicity (%)",
                rows=6:8) %>%
  tab_row_group(label="Language region (%)",
                rows=9:12) %>%
  tab_row_group(label="Maternal nationality (%)",
                rows=13:19) %>%
  tab_row_group(label="Civil status (%)",
                rows=20:21) %>%
  tab_row_group(label="Continuous variables",
                rows=22:24) %>%
  tab_row_group(label="Neonatal outcomes",
                rows=25:31) %>%
  row_group_order(groups=c("Parity (%)", "Urbanicity (%)", "Language region (%)", 
                           "Maternal nationality (%)", "Civil status (%)", "Continuous variables", "Neonatal outcomes")
  )%>%
  cols_label(name = "Birthyear") %>%
  cols_width(name ~ px(140), everything() ~ px(50)) %>%
  tab_options(table.width = 0.1, data_row.padding = px(1),   table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  )

combined_in6_inva %>%
  gtsave(here("output/tables_paper", "maternal_neonatal_characteristics_in6.html")) 
combined_in6_inva %>% 
  gtsave(here("output/tables_paper", "maternal_neonatal_characteristics_in6.docx"), encoding = "latin1")


# dataset bev_eco_in7 without stillbirth
formattedcomb7_inv <- formattedcomb7
combined_in7_inv <- t(formattedcomb7_inv)
rownames(combined_in7_inv) <- colnames(formattedcomb7_inv)
colnames(combined_in7_inv) <- rownames(formattedcomb7_inv)
combined_in7_inv

colnames(combined_in7_inv) <- combined_in7_inv[1,]
combined_in7_inv <- combined_in7_inv[-1, ]
combined_in7_inv
combined_in7_inv <- as_tibble(combined_in7_inv)
combined_in7_inv

combined_in7_inva <- add_column(combined_in7_inv, .before = 1,
                                name = c("1", "2", "3", ">3", "missing", 
                                         "Rural", "Urban", "missing",
                                         "German or Romansh", "French", "Italian", "missing",
                                         "Switzerland", "Africa", "Asia", "Europe", "Northern America", "Southern and Central America", "Missing or Oceania", 
                                         "Married", "Single",
                                         "Maternal age (years): mean ± sd", "Mean SSEP: mean ± sd", "Mean altitude (MASL): mean ± sd", 
                                         "Birthweight (g), mean ± sd", "Gestational age (weeks), mean ± sd", 
                                         "Female (%)", "Male (%)", "Preterm birth (%)", "Low birthweight (%)",
                                         "n")
)


combined_in7_inva <- combined_in7_inva %>%
  gt() %>%
  tab_header(title=md("**Maternal and neonatal characteristics of the analysed population (excl. stillbirths)**"), 
             subtitle = "n=1,269,586") %>%
  tab_row_group(label="Parity (%)",
                rows=1:5) %>%
  tab_row_group(label="Urbanicity (%)",
                rows=6:8) %>%
  tab_row_group(label="Language region (%)",
                rows=9:12) %>%
  tab_row_group(label="Maternal nationality (%)",
                rows=13:19) %>%
  tab_row_group(label="Civil status (%)",
                rows=20:21) %>%
  tab_row_group(label="Continuous variables",
                rows=22:24) %>%
  tab_row_group(label="Neonatal outcomes",
                rows=25:31) %>%
  row_group_order(groups=c("Parity (%)", "Urbanicity (%)", "Language region (%)", 
                           "Maternal nationality (%)", "Civil status (%)", "Continuous variables", "Neonatal outcomes")
  )%>%
  cols_label(name = "Birthyear") %>%
  cols_width(name ~ px(140), everything() ~ px(50)) %>%
  tab_options(table.width = 0.1, data_row.padding = px(1),   table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  )

combined_in7_inva %>%
  gtsave(here("output/tables_paper", "maternal_neonatal_characteristics_in7.html")) 
combined_in7_inva %>% 
  gtsave(here("output/tables_paper", "maternal_neonatal_characteristics_in7.docx"), encoding = "latin1")


#####

# Birthrate among permanent residents
birthrate_perm_pop <- t(formatted_br)
rownames(birthrate_perm_pop) <- colnames(formatted_br)
colnames(birthrate_perm_pop) <- rownames(formatted_br)
birthrate_perm_pop

birthrate_perm_pop[3,]<- format(round(birthrate_perm_pop[3,], digits=2), nsmall = 2)

#removing the first row that is useless, to have fir row (and col names) as birthyear
colnames(birthrate_perm_pop) <- birthrate_perm_pop[1,]
birthrate_perm_pop <- birthrate_perm_pop[-1, ] 
birthrate_perm_pop
birthrate_perm_pop <- as_tibble(birthrate_perm_pop)


birthrate_perm_pop <- add_column(birthrate_perm_pop, .before = 1,
                         name = c("n", "Birthrate (/1000 inhabitants)"
                         )
)
birthrate_perm_pop

birthrate_perm_pop %>%
  gt() %>%
  tab_header(title=md("**Birthrate among the permanent resident population**"),
             subtitle= "n=1,340,047") %>%
  cols_label(name = "Birthyear") %>%
  cols_width(name ~ px(120), everything() ~ px(50)) %>%
  fmt_number(
    columns = everything(),
    rows = c(1),
    decimals = 0
  ) %>%
  tab_options(table.width = 0.1, data_row.padding = px(1), 
              heading.title.font.size = 14, 
              heading.subtitle.font.size = 14,
              table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              
  ) %>%
  gtsave(here("output/tables_paper", "birthrate_resident_pop.html"))
birthrate_perm_pop

birthrate_perm_pop %>%
  gt()%>%
  gtsave(here("output/tables_paper", "birthrate_resident_pop.docx"))


# GA_weeks
### V2: grouped rows
GA_weeks_birthyear <- t(formattedd)
rownames(GA_weeks_birthyear) <- colnames(formattedd)
colnames(GA_weeks_birthyear) <- rownames(formattedd)
GA_weeks_birthyear

#removing the first row that is useless, to have fir row (and col names) as birthyear
colnames(GA_weeks_birthyear) <- GA_weeks_birthyear[1,]
GA_weeks_birthyear <- GA_weeks_birthyear[-1, ] 
GA_weeks_birthyear
GA_weeks_birthyear <- as_tibble(GA_weeks_birthyear)
GA_weeks_birthyear

GA_weeks_birthyear <- add_column(GA_weeks_birthyear, .before = 1,
                          name = c("37-38", "38-39", 
                                   "39-40", "40-41", ">=41")
)
GA_weeks_birthyear


GA_weeks_birthyear <- GA_weeks_birthyear %>%
  gt() %>%
  tab_header(title=md("**Gestational age of the analysed population (excl. stillbirths)**"),
             subtitle = "n=1'202'936")%>%
  cols_label(name = "Birthyear") %>%
  cols_width(name ~ px(120), everything() ~ px(50)) %>%
  tab_options(table.width = 0.1, data_row.padding = px(1),   table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  ) %>%
  tab_row_group(label="Gestational age (weeks)",
                rows=1:5) 

GA_weeks_birthyear %>%
  gtsave(here("output/tables_paper/suppl_material", "GA_weeks_birthyear_in7.html"))

GA_weeks_birthyear%>%
  gtsave(here("output/tables_paper/suppl_material", "GA_weeks_birthyear_in7.docx"))

table(bevn_eco$Language, bevn_eco$mother_nationality_cat2)
round(prop.table(table(bevn_eco$mother_nationality_cat2, bevn_eco$Language,useNA="always"), margin=1)*100,2)
