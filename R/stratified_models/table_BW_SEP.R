#Before: RUN THE R FILE: PTB_GAM_stratified_by_Language_Region.Rmd

#test with suppl column for Crises
## bw and language region
HW_low<-as.vector(as.matrix(tab_m_BW_low[15,]))
HW_low
GR_low<-as.vector(as.matrix(tab_m_BW_low[16,]))
GR_low
first_wave_low<-as.vector(as.matrix(tab_m_BW_low[17,]))
first_wave_low
second_wave_low<-as.vector(as.matrix(tab_m_BW_low[18,]))
second_wave_low

HW_medium<-as.vector(as.matrix(tab_m_BW_medium[15,]))
HW_medium
GR_medium<-as.vector(as.matrix(tab_m_BW_medium[16,]))
GR_medium
first_wave_medium<-as.vector(as.matrix(tab_m_BW_medium[17,]))
first_wave_medium
second_wave_medium<-as.vector(as.matrix(tab_m_BW_medium[18,]))
second_wave_medium

HW_high<-as.vector(as.matrix(tab_m_BW_high[15,]))
HW_high
GR_high<-as.vector(as.matrix(tab_m_BW_high[16,]))
GR_high
first_wave_high<-as.vector(as.matrix(tab_m_BW_high[17,]))
first_wave_high
second_wave_high<-as.vector(as.matrix(tab_m_BW_high[18,]))
second_wave_high

BW_estimates_crises_SEP <- data.frame(rbind(HW_low, GR_low, first_wave_low, second_wave_low,
                                         HW_medium,  GR_medium, first_wave_medium, second_wave_medium,
                                         HW_high, GR_high, first_wave_high, second_wave_high))


BW_estimates_crises_SEP['SEP'] <- c("low", " ", " ", " ", "medium", " ", " ", " ", "high", " ", " ", " ")
BW_estimates_crises_SEP['Crises'] <- c("Heatwave", "Great Recession", "Covid: first wave", "Covid: second wave",
                                    "Heatwave", "Great Recession", "Covid: first wave", "Covid: second wave",
                                    "Heatwave", "Great Recession", "Covid: first wave", "Covid: second wave")

BW_estimates_crises_SEP1<-BW_estimates_crises_SEP[, c("SEP", "Crises", "X1","X2", "X3", "X4")]
BW_estimates_crises_SEP1 <- BW_estimates_crises_SEP1 %>%
  setNames(c("SEP","Crises", "estimate (g)", "lci", "uci", "d"))


BW_estimates_crises_SEP1 <- setDT(BW_estimates_crises_SEP1, keep.rownames = FALSE)

BW_estimates_crises_Lang_Reg_gt <- BW_estimates_crises_Lang_Reg1 %>%
  gt()%>%
  tab_header(title="Birthweight, stratified by Language Region") %>%
  tab_spanner(label = "95% CI (g)", columns = c(lci, uci)) 
BW_estimates_crises_Lang_Reg_gt

BW_estimates_crises_Lang_Reg_gt <- BW_estimates_crises_Lang_Reg_gt%>%
  gtsave(here("output/stratification/tables", "BW_stratified_by_SEP_crises_effect.html"))
