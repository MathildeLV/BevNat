#Before: RUN THE R FILE: BW_GAM_stratified_by_Language_Region.Rmd

#test with suppl column for Crises
## bw and language region
HW_German<-as.vector(as.matrix(tab_m_BW_German[13,]))
HW_German
GR_German<-as.vector(as.matrix(tab_m_BW_German[14,]))
GR_German
first_wave_German<-as.vector(as.matrix(tab_m_BW_German[15,]))
first_wave_German
second_wave_German<-as.vector(as.matrix(tab_m_BW_German[16,]))
second_wave_German

HW_French<-as.vector(as.matrix(tab_m_BW_French[13,]))
HW_French
GR_French<-as.vector(as.matrix(tab_m_BW_French[14,]))
GR_French
first_wave_French<-as.vector(as.matrix(tab_m_BW_French[15,]))
first_wave_French
second_wave_French<-as.vector(as.matrix(tab_m_BW_French[16,]))
second_wave_French

HW_Italian<-as.vector(as.matrix(tab_m_BW_Italian[13,]))
HW_Italian
GR_Italian<-as.vector(as.matrix(tab_m_BW_Italian[14,]))
GR_Italian
first_wave_Italian<-as.vector(as.matrix(tab_m_BW_Italian[15,]))
first_wave_Italian
second_wave_Italian<-as.vector(as.matrix(tab_m_BW_Italian[16,]))
second_wave_Italian

BW_estimates_crises_Lang_Reg <- data.frame(rbind(HW_German, GR_German, first_wave_German, second_wave_German,
                                         HW_French,  GR_French, first_wave_French, second_wave_French,
                                         HW_Italian, GR_Italian, first_wave_Italian, second_wave_Italian))


BW_estimates_crises_Lang_Reg['Language Region'] <- c("German", " ", " ", " ", "French", " ", " ", " ", "Italian", " ", " ", " ")
BW_estimates_crises_Lang_Reg['Crises'] <- c("Heatwave", "Great Recession", "Covid: first wave", "Covid: second wave",
                                    "Heatwave", "Great Recession", "Covid: first wave", "Covid: second wave",
                                    "Heatwave", "Great Recession", "Covid: first wave", "Covid: second wave")

BW_estimates_crises_Lang_Reg1<-BW_estimates_crises_Lang_Reg[, c("Language Region", "Crises", "X1","X2", "X3", "X4")]
BW_estimates_crises_Lang_Reg1 <- BW_estimates_crises_Lang_Reg1 %>%
  setNames(c("Language Region","Crises", "estimate (g)", "lci", "uci", "d"))


BW_estimates_crises_Lang_Reg1 <- setDT(BW_estimates_crises_Lang_Reg1, keep.rownames = FALSE)

BW_estimates_crises_Lang_Reg_gt <- BW_estimates_crises_Lang_Reg1 %>%
  gt()%>%
  tab_header(title="Birthweight, stratified by Language Region") %>%
  tab_spanner(label = "95% CI (g)", columns = c(lci, uci)) 
BW_estimates_crises_Lang_Reg_gt

BW_estimates_crises_Lang_Reg_gt <- BW_estimates_crises_Lang_Reg_gt%>%
  gtsave(here("output/stratification/tables", "BW_stratified_by_Lang_Reg_crises_effect.html"))
