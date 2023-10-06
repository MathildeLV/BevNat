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
                rows=12:19) %>%
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


# Maternal charact: exposure to each crisis, during delivery or during each pregnancy trimester
## In dataset 0
# mat_crises_in <- t(formattedcr_eco)
# rownames(mat_crises_in) <- colnames(formattedcr_eco)
# colnames(mat_crises_in) <- rownames(formattedcr_eco)
# mat_crises_in
# 
# #removing the first row that is useless, to have fir row (and col names) as birthyear
# colnames(mat_crises_in) <- mat_crises_in[1,]
# mat_crises_in <- mat_crises_in[-1, ]
# mat_crises_in
# mat_crises_in <- as_tibble(mat_crises_in)
# mat_crises_in
# 
# mat_in_crises <- add_column(mat_crises_in, .before = 1,
#                           name = c("Heatwave", "Great Recession", "COVID-19",
#                                    "Heatwave", "Great Recession", "COVID-19",
#                                    "Heatwave", "Great Recession", "COVID-19",
#                                    "Heatwave", "Great Recession", "COVID-19", "n")
# )
# mat_in_crises
# 
# mat_in_crises %>%
#   gt() %>%
#   tab_header(title=md("**Maternal exposure to each crises depending on the pregnancy trimester**"),
#              subtitle = "n=1,425,757")%>%
#   cols_label(name = "Birthyear") %>%
#   tab_row_group(label="Third trimester: n (%)",
#                 rows=10:12) %>%
#   tab_row_group(label="Second trimester: n (%)",
#                 rows=7:9) %>%
#   tab_row_group(label="First trimester: n (%)",
#                 rows=4:6) %>%
#   tab_row_group(label="Time of delivery: n (%)",
#                 rows=1:3) %>%
#   cols_width(name ~ px(120), everything() ~ px(50)) %>%
#   tab_options(table.width = 0.1, data_row.padding = px(1),   table.font.size = 12,
#               column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
#               heading.title.font.size = 14, heading.subtitle.font.size = 14
#   ) %>%
#   row_group_order(groups = c(NA, "Time of delivery: n (%)", "First trimester: n (%)", "Second trimester: n (%)","Third trimester: n (%)")) %>%
#   gtsave(here("output/tables_paper", "mat_exposure_crises_in.html"))
# mat_in_crises

#### without grouping by year
mat_crises_in_b <- t(formattedcr_eco_b)
rownames(mat_crises_in_b) <- colnames(formattedcr_eco_b)
colnames(mat_crises_in_b) <- rownames(formattedcr_eco_b)
mat_crises_in_b

colnames(mat_crises_in_b) <- "n (%)"
# mat_crises_in_b <- mat_crises_in_b[-1, ] 
# mat_crises_in_b
mat_crises_in_b <- as_tibble(mat_crises_in_b)
mat_crises_in_b

mat_crises_in_b_t <- add_column(mat_crises_in_b, .before = 1,
                            "Timing of exposure" = c("Heatwave (2015 and 2018)", "Great Recession (2008-2009)", "COVID-19 (2020-2021)",
                                     "Heatwave (2015 and 2018)", "Great Recession (2008-2009)", "COVID-19 (2020-2021)",
                                     "Heatwave (2015 and 2018)", "Great Recession (2008-2009)", "COVID-19 (2020-2021)", 
                                     "Heatwave (2015 and 2018)", "Great Recession (2008-2009)", "COVID-19 (2020-2021)")
)
mat_crises_in_b_t 


mat_crises_in_b_t %>%
  gt() %>%
  tab_header(title=md("**Mothers exposed to each crisis depending on the pregnancy trimester**"),
             subtitle = "n=1,425,757 (eligible population)")%>%
  tab_row_group(label="Third trimester",
                rows=10:12) %>%
  tab_row_group(label="Second trimester",
                rows=7:9) %>%
  tab_row_group(label="First trimester",
                rows=4:6) %>%
  tab_row_group(label="Delivery",
                rows=1:3) %>%
  # cols_label(name= "Timing of exposure") %>%
  # cols_width(name ~ px(120), everything() ~ px(50)) %>%
  tab_options(table.width = pct(17), data_row.padding = px(1),   table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  ) %>%
  gtsave(here("output/tables_paper", "mat_exposure_crises_in_not_by_year.html"))
mat_crises_in_b_t

mat_crises_in_b_t %>%
  gt()%>%
  tab_header(title=md("**Mothers exposed to each crisis depending on the pregnancy trimester**"),
             subtitle = "n=1,425,757 (eligible population)")%>%
  tab_row_group(label="Third trimester",
                rows=10:12) %>%
  tab_row_group(label="Second trimester",
                rows=7:9) %>%
  tab_row_group(label="First trimester",
                rows=4:6) %>%
  tab_row_group(label="Delivery",
                rows=1:3) %>%
  tab_options(table.width = pct(17), data_row.padding = px(1),   table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  ) %>%
  gtsave(here("output/tables_paper", "mat_exposure_crises_in_not_by_year.docx"))


### In data set 7, without grouping by year
mat_crises_in7_b <- t(formattedcr_eco_b_in7)
rownames(mat_crises_in7_b) <- colnames(formattedcr_eco_b_in7)
colnames(mat_crises_in7_b) <- rownames(formattedcr_eco_b_in7)
mat_crises_in7_b

colnames(mat_crises_in7_b) <- "n (%)"
mat_crises_in7_b <- as_tibble(mat_crises_in7_b)
mat_crises_in7_b

mat_crises_in7_b <- add_column(mat_crises_in7_b, .before = 1,
                                "Timing of exposure" = c("Heatwave (2015 and 2018)", "Great Recession (2008-2009)", "COVID-19 (2020-2021)",
                                                         "Heatwave (2015 and 2018)", "Great Recession (2008-2009)", "COVID-19 (2020-2021)",
                                                         "Heatwave (2015 and 2018)", "Great Recession (2008-2009)", "COVID-19 (2020-2021)", 
                                                         "Heatwave (2015 and 2018)", "Great Recession (2008-2009)", "COVID-19 (2020-2021)")
)
mat_crises_in7_b 

mat_crises_in7_b %>%
  gt() %>%
  tab_header(title=md("**Mothers exposed to each crisis depending on the pregnancy trimester**"),
             subtitle = gt::html("n=1,190,244 (analysed population, <br> (excl. stillbirths))"))%>%
  tab_row_group(label="Third trimester",
                rows=10:12) %>%
  tab_row_group(label="Second trimester",
                rows=7:9) %>%
  tab_row_group(label="First trimester",
                rows=4:6) %>%
  tab_row_group(label="Delivery",
                rows=1:3) %>%
  # cols_label(name= "Timing of exposure") %>%
  # cols_width(name ~ px(120), everything() ~ px(50)) %>%
  tab_options(table.width = pct(17), data_row.padding = px(1),   table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  ) %>%
  gtsave(here("output/tables_paper", "mat_exposure_crises_in7_not_by_year.html")) 
mat_crises_in7_b

mat_crises_in7_b %>%
  gt()%>%
  tab_header(title=md("**Mothers exposed to each crisis depending on the pregnancy trimester**"),
             subtitle = gt::html("n=1,190,244 (analysed population, <br> (excl. stillbirths))"))%>%
  tab_row_group(label="Third trimester",
                rows=10:12) %>%
  tab_row_group(label="Second trimester",
                rows=7:9) %>%
  tab_row_group(label="First trimester",
                rows=4:6) %>%
  tab_row_group(label="Delivery",
                rows=1:3) %>%
  tab_options(table.width = pct(17), data_row.padding = px(1),   table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  ) %>%
  gtsave(here("output/tables_paper", "mat_exposure_crises_in7_not_by_year.docx"))


### In data set 6, without grouping by year
mat_crises_in6_b <- t(formattedcr_eco_b_in6)
rownames(mat_crises_in6_b) <- colnames(formattedcr_eco_b_in6)
colnames(mat_crises_in6_b) <- rownames(formattedcr_eco_b_in6)
mat_crises_in6_b

colnames(mat_crises_in6_b) <- "n (%)"
mat_crises_in6_b <- as_tibble(mat_crises_in6_b)
mat_crises_in6_b

mat_crises_in6_b <- add_column(mat_crises_in6_b, .before = 1,
                               "Timing of exposure" = c("Heatwave (2015 and 2018)", "Great Recession (2008-2009)", "COVID-19 (2020-2021)",
                                                        "Heatwave (2015 and 2018)", "Great Recession (2008-2009)", "COVID-19 (2020-2021)",
                                                        "Heatwave (2015 and 2018)", "Great Recession (2008-2009)", "COVID-19 (2020-2021)", 
                                                        "Heatwave (2015 and 2018)", "Great Recession (2008-2009)", "COVID-19 (2020-2021)")
)
mat_crises_in6_b 

mat_crises_in6_b %>%
  gt() %>%
  tab_header(title=md("**Mothers exposed to each crisis depending on the pregnancy trimester**"),
             subtitle = gt::html("n=1,194,742 (analysed population, <br> (incl. stillbirths))"))%>%  #html used to put a line break <br>
  tab_row_group(label="Third trimester",
                rows=10:12) %>%
  tab_row_group(label="Second trimester",
                rows=7:9) %>%
  tab_row_group(label="First trimester",
                rows=4:6) %>%
  tab_row_group(label="Delivery",
                rows=1:3) %>%
 tab_options(table.width = pct(17), data_row.padding = px(1),   table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  ) %>%
  gtsave(here("output/tables_paper", "mat_exposure_crises_in6_not_by_year.html"))
mat_crises_in6_b

mat_crises_in6_b %>%
  gt()%>%
  tab_header(title=md("**Mothers exposed to each crisis depending on the pregnancy trimester**"),
             subtitle = gt::html("n=1,194,742 (analysed population, <br> (incl. stillbirths))"))%>%  #html used to put a line break <br>
  tab_row_group(label="Third trimester",
                rows=10:12) %>%
  tab_row_group(label="Second trimester",
                rows=7:9) %>%
  tab_row_group(label="First trimester",
                rows=4:6) %>%
  tab_row_group(label="Delivery",
                rows=1:3) %>%
  tab_options(table.width = pct(17), data_row.padding = px(1),   table.font.size = 12,
              column_labels.font.weight = "bold",  row_group.font.weight  = "bold",
              heading.title.font.size = 14, heading.subtitle.font.size = 14
  ) %>%
  gtsave(here("output/tables_paper", "mat_exposure_crises_in6_not_by_year.docx"))




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
             subtitle= "n=1,257,826") %>%
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
