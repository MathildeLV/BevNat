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
                            "missing  (%)", "Mother nationalty (cat1): Switzerland  (%)", "Other  (%)", "Mother nationality (cat2): Switzerland  (%)", "Africa  (%)", "Asia  (%)", "Europe  (%)", 
                            "Northern America  (%)", "Southern and Central America  (%)", "Oceania  (%)", "Missing  (%)", "Urbanicity: Rural  (%)",
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
                            "missing", "Switzerland", "Other", "Switzerland", "Africa", "Asia", "Europe", 
                            "Northern America", "Southern and Central America", "Oceania", "Missing", "Rural",
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
  tab_row_group(label="Maternal Nationality: cat1",
    rows=14:15) %>%
  tab_row_group(label="Maternal Nationality: cat2",
    rows=16:23) %>%
  tab_row_group(label="Urbanicity",
    rows=24:25) %>%
  tab_row_group(label="Language region",
    rows=26:28) %>%
  row_group_order(groups=c("Maternal age (categorical)", "Parity", "Maternal Nationality: cat1", "Maternal Nationality: cat2",
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
                                     "missing", "Switzerland", "Other", "Switzerland", "Africa", "Asia", "Europe", 
                                     "Northern America", "Southern and Central America", "Oceania", "Missing", "Rural",
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
  tab_row_group(label="Maternal Nationality: cat1",
                rows=14:15) %>%
  tab_row_group(label="Maternal Nationality: cat2",
                rows=16:23) %>%
  tab_row_group(label="Urbanicity",
                rows=24:25) %>%
  tab_row_group(label="Language region",
                rows=26:28) %>%
  row_group_order(groups=c("Maternal age (categorical)", "Parity", "Maternal Nationality: cat1", "Maternal Nationality: cat2",
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
                                    "missing", "Switzerland", "Other", "Switzerland", "Africa", "Asia", "Europe", 
                                    "Northern America", "Southern and Central America", "Oceania", "Missing", "Rural",
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
  tab_row_group(label="Maternal Nationality: cat1",
                rows=14:15) %>%
  tab_row_group(label="Maternal Nationality: cat2",
                rows=16:23) %>%
  tab_row_group(label="Urbanicity",
                rows=24:25) %>%
  tab_row_group(label="Language region",
                rows=26:28) %>%
  row_group_order(groups=c("Maternal age (categorical)", "Parity", "Maternal Nationality: cat1", "Maternal Nationality: cat2",
                           "Urbanicity", "Language region")
  )%>%
  cols_label(name = "birthyear")
mat_in_inv_7

mat_in_inv_7 %>%
  gtsave(here("output/tables", "maternal_characteristics_in7_inverted.html"))



# Neonatal characteristics
## At inclusion : in