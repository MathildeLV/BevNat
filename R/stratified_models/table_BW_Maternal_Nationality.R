#Before: RUN THE R FILE: BW_GAM_stratified_by_maternal_nationality.Rmd

#test with suppl column for Crises
## BW and maternal nationality
HW_Swiss<-as.vector(as.matrix(tab_m_BW_bam_Swiss [10,]))
GR_Swiss<-as.vector(as.matrix(tab_m_BW_bam_Swiss[11,]))
first_wave_Swiss<-as.vector(as.matrix(tab_m_BW_bam_Swiss[12,]))
second_wave_Swiss<-as.vector(as.matrix(tab_m_BW_bam_Swiss[13,]))

HW_African<-as.vector(as.matrix(tab_m_BW_bam_Afr[10,]))
GR_African<-as.vector(as.matrix(tab_m_BW_bam_Afr[11,]))
first_wave_African<-as.vector(as.matrix(tab_m_BW_bam_Afr[12,]))
second_wave_African<-as.vector(as.matrix(tab_m_BW_bam_Afr[13,]))

HW_Asian<-as.vector(as.matrix(tab_m_BW_bam_Asi[10,]))
GR_Asian<-as.vector(as.matrix(tab_m_BW_bam_Asi[11,]))
first_wave_Asian<-as.vector(as.matrix(tab_m_BW_bam_Asi[12,]))
second_wave_Asian<-as.vector(as.matrix(tab_m_BW_bam_Asi[13,]))

HW_Eur<-as.vector(as.matrix(tab_m_BW_bam_Eur[10,]))
GR_Eur<-as.vector(as.matrix(tab_m_BW_bam_Eur[11,]))
first_wave_Eur<-as.vector(as.matrix(tab_m_BW_bam_Eur[12,]))
second_wave_Eur<-as.vector(as.matrix(tab_m_BW_bam_Eur[13,]))

HW_Nort_Am<-as.vector(as.matrix(tab_m_BW_Nort_Am[10,]))
GR_Nort_Am<-as.vector(as.matrix(tab_m_BW_Nort_Am[11,]))
first_wave_Nort_Am<-as.vector(as.matrix(tab_m_BW_Nort_Am[12,]))
second_wave_Nort_Am<-as.vector(as.matrix(tab_m_BW_Nort_Am[13,]))

HW_S_C_Am<-as.vector(as.matrix(tab_m_BW_S_C_Am [10,]))
GR_S_C_Am<-as.vector(as.matrix(tab_m_BW_S_C_Am [11,]))
first_wave_S_C_Am<-as.vector(as.matrix(tab_m_BW_S_C_Am [12,]))
second_wave_S_C_Am<-as.vector(as.matrix(tab_m_BW_S_C_Am [13,]))

BW_estimates_crises_nationality <- data.frame(rbind(HW_Swiss, GR_Swiss, first_wave_Swiss, second_wave_Swiss,
                                         HW_African, GR_African, first_wave_African, second_wave_African,
                                         HW_Asian, GR_Asian, first_wave_Asian, second_wave_Asian,
                                         HW_Eur, GR_Eur, first_wave_Eur, second_wave_Eur,
                                         HW_Nort_Am, GR_Nort_Am, first_wave_Nort_Am, second_wave_Nort_Am,
                                         HW_S_C_Am, GR_S_C_Am, first_wave_S_C_Am, second_wave_S_C_Am
                                          ))

BW_estimates_crises_nationality['Maternal Nationality'] <- c("Swiss", " ", " ", " ", "African", " ", " ", " ",
                                                              "Asian", " ", " ", " ", "European", " ", " ", " ",
                                                              "Northern American", " ", " ", " ", "Southern/Central American", " ", " ", " ")

BW_estimates_crises_nationality['Crises'] <- c("Heatwave", "Great Recession", "Covid: first wave", "Covid: second wave",
                                    "Heatwave", "Great Recession", "Covid: first wave", "Covid: second wave",
                                    "Heatwave", "Great Recession", "Covid: first wave", "Covid: second wave")

BW_estimates_crises_nationality1<-BW_estimates_crises_nationality[, c("Maternal Nationality", "Crises", "X1","X2", "X3", "X4")]
BW_estimates_crises_nationality1 <- BW_estimates_crises_nationality1 %>%
  setNames(c("Maternal Nationality","Crises", "estimate (g)", "lci", "uci", "d"))

BW_estimates_crises_nationality1 <- setDT(BW_estimates_crises_nationality1, keep.rownames = FALSE)

BW_estimates_crises_nationality1 <- BW_estimates_crises_nationality1 %>%
  gt()%>%
  tab_header(title="Birthweight, stratified by maternal nationality") %>%
  tab_spanner(label = "95% CI (g)", columns = c(lci, uci)) 
BW_estimates_crises_nationality1

BW_estimates_crises_nationality1 <- BW_estimates_crises_nationality1%>%
  gtsave(here("output/stratification/tables", "BW_stratified_by_nationality_crises_effect.html"))
