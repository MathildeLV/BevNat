#Before: RUN THE R FILE: PTB_GAM_stratified_by_Language_Region.Rmd

#test with suppl column for Crises
## PTB and language region
HW_German<-as.vector(as.matrix(tab_m_PTB_bam_German[13,]))
HW_German
GR_German<-as.vector(as.matrix(tab_m_PTB_bam_German[14,]))
GR_German
first_wave_German<-as.vector(as.matrix(tab_m_PTB_bam_German[15,]))
first_wave_German
second_wave_German<-as.vector(as.matrix(tab_m_PTB_bam_German[16,]))
second_wave_German

HW_French<-as.vector(as.matrix(tab_m_PTB_bam_French[13,]))
HW_French
GR_French<-as.vector(as.matrix(tab_m_PTB_bam_French[14,]))
GR_French
first_wave_French<-as.vector(as.matrix(tab_m_PTB_bam_French[15,]))
first_wave_French
second_wave_French<-as.vector(as.matrix(tab_m_PTB_bam_French[16,]))
second_wave_French

HW_Italian<-as.vector(as.matrix(tab_m_PTB_bam_Italian[13,]))
HW_Italian
GR_Italian<-as.vector(as.matrix(tab_m_PTB_bam_Italian[14,]))
GR_Italian
first_wave_Italian<-as.vector(as.matrix(tab_m_PTB_bam_Italian[15,]))
first_wave_Italian
second_wave_Italian<-as.vector(as.matrix(tab_m_PTB_bam_Italian[16,]))
second_wave_Italian

PTB_estimates_crises_Lang_Reg <- data.frame(rbind(HW_German, GR_German, first_wave_German, second_wave_German,
                                         HW_French,  GR_French, first_wave_French, second_wave_French,
                                         HW_Italian, GR_Italian, first_wave_Italian, second_wave_Italian))


PTB_estimates_crises_Lang_Reg['Language Region'] <- c("German", " ", " ", " ", "French", " ", " ", " ", "Italian", " ", " ", " ")
PTB_estimates_crises_Lang_Reg['Crises'] <- c("Heatwave", "Great Recession", "Covid: first wave", "Covid: second wave",
                                    "Heatwave", "Great Recession", "Covid: first wave", "Covid: second wave",
                                    "Heatwave", "Great Recession", "Covid: first wave", "Covid: second wave")

PTB_estimates_crises_Lang_Reg1<-PTB_estimates_crises_Lang_Reg[, c("Language Region", "Crises", "X1","X2", "X3", "X4")]
PTB_estimates_crises_Lang_Reg1 <- PTB_estimates_crises_Lang_Reg1 %>%
  setNames(c("Language Region","Crises", "OR", "lci", "uci", "d"))


PTB_estimates_crises_Lang_Reg1 <- setDT(PTB_estimates_crises_Lang_Reg1, keep.rownames = FALSE)

PTB_estimates_crises_Lang_Reg_gt <- PTB_estimates_crises_Lang_Reg1 %>%
  gt()%>%
  tab_header(title="Preterm birth risk, stratified by Language Region") %>%
  tab_spanner(label = "95% CI", columns = c(lci, uci)) 
PTB_estimates_crises_Lang_Reg_gt

PTB_estimates_crises_Lang_Reg_gt <- PTB_estimates_crises_Lang_Reg_gt%>%
  gtsave(here("output/stratification/tables", "PTB_stratified_by_Lang_Reg_crises_effect.html"))
