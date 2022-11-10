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


mat_in_inv_v1 <- add_column(mat_in_inv, .before = 1,
                   name = c("n", "Maternal age (years): mean ± sd ", "Maternal age category: 
           (10-20]  (%)", "(20-25]  (%) ", "(25-30]  (%)", "(30-35]  (%)", "(35-40]  (%)", "> 40  (%)", "Parity: 1  (%)", "2  (%)", "3  (%)", ">  (%)3", 
                            "missing  (%)", "Mother nationality (cat2): Switzerland  (%)", "Africa  (%)", "Asia  (%)", "Europe  (%)", 
                            "Northern America  (%)", "Southern and Central America  (%)", "Missing  or Oceania (%)", "Urbanicity: Rural  (%)",
                            "Urban  (%)", "Language region: German or Romansh  (%)", "French  (%)", "Italian  (%)", "Mean SSEP: mean ± sd ")
)
mat_in_inv_v1

mat_in_inv_v1 <- mat_in_inv_v1%>%
   gt()%>%
  tab_header(title="Maternal characteristics at inclusion")%>%
   cols_label(name = "birthyear")
mat_in_inv_v1

mat_in_inv_v1 %>%
  gtsave(here("output/tables", "maternal_characteristics_in_inverted_v1.html"))


### V2: grouped rows
mat_in_inv_v2 <- add_column(mat_in_inv, .before = 1,
                   name = c("n", "Maternal age (years) ", "(10-20]", 
                            "(20-25]", "(25-30]", "(30-35]", "(35-40]", "> 40", "1", "2", "3", ">3", 
                            "missing", "Switzerland", "Africa", "Asia", "Europe", 
                            "Northern America", "Southern and Central America", "Missing or Oceania", "Rural",
                            "Urban", "German or Romansh", "French", "Italian", "Mean SSEP")
)

mat_in_inv_v2

mat_in_inv_v2 <- mat_in_inv_v2 %>%
  gt() %>%
  tab_header(title="Maternal characteristics at inclusion") %>%
  tab_row_group(label = "Maternal age (categorical)",
    rows = 3:8) %>%
  tab_row_group(label="Parity",
    rows=9:13) %>%
  tab_row_group(label="Maternal Nationality: cat2",
    rows=14:20) %>%
  tab_row_group(label="Urbanicity",
    rows=21:22) %>%
  tab_row_group(label="Language region",
    rows=23:26) %>%
  row_group_order(groups=c("Maternal age (categorical)", "Parity", "Maternal Nationality: cat2",
                           "Urbanicity", "Language region")
                           )%>%
  cols_label(name = "birthyear")
mat_in_inv_v2

mat_in_inv_v2 %>%
  gtsave(here("output/tables", "maternal_characteristics_in_inverted_v2.html"))



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
                            name = c("n", "Maternal age (years) ", "(10-20]", 
                                     "(20-25]", "(25-30]", "(30-35]", "(35-40]", "> 40", "1", "2", "3", ">3", 
                                     "missing", "Switzerland", "Africa", "Asia", "Europe", 
                                     "Northern America", "Southern and Central America", "Missing or Oceania", "Rural",
                                     "Urban", "German or Romansh", "French", "Italian", "Mean SSEP")
)

mat_in_inv_6

mat_in_inv_6 <- mat_in_inv_6 %>%
  gt() %>%
  tab_header(title="Maternal characteristics in dataset in6") %>%
  tab_row_group(label = "Maternal age (categorical)",
                rows = 3:8) %>%
  tab_row_group(label="Parity",
                rows=9:13) %>%
  tab_row_group(label="Maternal Nationality: cat2",
                rows=14:20) %>%
  tab_row_group(label="Urbanicity",
                rows=21:22) %>%
  tab_row_group(label="Language region",
                rows=23:26) %>%
  row_group_order(groups=c("Maternal age (categorical)", "Parity", "Maternal Nationality: cat2",
                           "Urbanicity", "Language region")
  )%>%
  cols_label(name = "birthyear")
mat_in_inv_6

mat_in_inv_6 %>%
  gtsave(here("output/tables", "maternal_characteristics_in6_inverted.html"))




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
                           name = c("n", "Maternal age (years) ", "(10-20]", 
                                    "(20-25]", "(25-30]", "(30-35]", "(35-40]", "> 40", "1", "2", "3", ">3", 
                                    "missing", "Switzerland", "Africa", "Asia", "Europe", 
                                    "Northern America", "Southern and Central America", "Missing or Oceania", "Rural",
                                    "Urban", "German or Romansh", "French", "Italian", "Mean SSEP")
)

mat_in_inv_7

mat_in_inv_7 <- mat_in_inv_7 %>%
  gt() %>%
  tab_header(title="Maternal characteristics in dataset in7") %>%
  tab_row_group(label = "Maternal age (categorical)",
                rows = 3:8) %>%
  tab_row_group(label="Parity",
                rows=9:13) %>%
  tab_row_group(label="Maternal Nationality: cat2",
                rows=14:20) %>%
  tab_row_group(label="Urbanicity",
                rows=21:22) %>%
  tab_row_group(label="Language region",
                rows=23:25) %>%
  row_group_order(groups=c("Maternal age (categorical)", "Parity", "Maternal Nationality: cat2",
                           "Urbanicity", "Language region")
  )%>%
  cols_label(name = "birthyear")
mat_in_inv_7

mat_in_inv_7 %>%
  gtsave(here("output/tables", "maternal_characteristics_in7_inverted.html"))



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
                                    "Female (%)", "Male (%)", "Stillbirth (%)", "Preterm birth (%)", "Low birthweight (%)", "Macrosomia (%)")
)

neo_in_inv

 neo_in_inv %>%
  gt() %>%
  tab_header(title="Neonatal characteristics at inclusion") %>%
    cols_label(name = "Birthyear") %>%
  gtsave(here("output/tables", "neonatal_characteristics_in_inverted.html"))
neo_in_inv


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
                                  "Female (%)", "Male (%)", "Stillbirth (%)", "Preterm birth (%)", "Low birthweight (%)", "Macrosomia (%)")
)
neo_in6_inv

neo_in6_inv %>%
  gt() %>%
  tab_header(title="Neonatal characteristics in dataset 6") %>%
  cols_label(name = "Birthyear") %>%
  gtsave(here("output/tables", "neonatal_characteristics_in6_inverted.html"))
neo_in6_inv

## In dataset 7
neo_in7_inv <- t(formatted1)
rownames(neo_in7_inv) <- colnames(formatted1)
colnames(v) <- rownames(formatted1)
neo_in7_inv

#removing the first row that is useless, to have fir row (and col names) as birthyear
colnames(neo_in7_inv) <- neo_in7_inv[1,]
neo_in7_inv <- neo_in7_inv[-1, ] 
neo_in7_inv
neo_in7_inv <- as_tibble(neo_in7_inv)
neo_in7_inv

neo_in7_inv <- add_column(neo_in7_inv, .before = 1,
                          name = c("n", "Birthweight (g), mean ± sd", "Gestational age (weeks), mean ± sd", 
                                   "Female (%)", "Male (%)", "Preterm birth (%)", "Low birthweight (%)", "Macrosomia (%)")
)
neo_in7_inv

neo_in7_inv %>%
  gt() %>%
  tab_header(title="Neonatal characteristics in dataset 6") %>%
  cols_label(name = "Birthyear") %>%
  gtsave(here("output/tables", "neonatal_characteristics_in7_inverted.html"))
neo_in7_inv
