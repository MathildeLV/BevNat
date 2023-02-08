# table
#test with suppl column for Crises
## PTB and SSEP
HW_low_SEP<-as.vector(as.matrix(tab_m_PTB_bam_L_SSEP1[15,]))
HW_low_SEP
GR_low_SEP<-as.vector(as.matrix(tab_m_PTB_bam_L_SSEP1[16,]))
GR_low_SEP
first_wave_low_SEP<-as.vector(as.matrix(tab_m_PTB_bam_L_SSEP1[17,]))
first_wave_low_SEP
second_wave_low_SEP<-as.vector(as.matrix(tab_m_PTB_bam_L_SSEP1[18,]))
second_wave_low_SEP

HW_medium_SEP<-as.vector(as.matrix(tab_m_PTB_bam_M_SSEP1[15,]))
HW_medium_SEP
GR_medium_SEP<-as.vector(as.matrix(tab_m_PTB_bam_M_SSEP1[16,]))
GR_medium_SEP
first_wave_medium_SEP<-as.vector(as.matrix(tab_m_PTB_bam_M_SSEP1[17,]))
first_wave_medium_SEP
second_wave_medium_SEP<-as.vector(as.matrix(tab_m_PTB_bam_M_SSEP1[18,]))
second_wave_medium_SEP

HW_high_SEP<-as.vector(as.matrix(tab_m_PTB_bam_H_SSEP1[15,]))
HW_high_SEP
GR_high_SEP<-as.vector(as.matrix(tab_m_PTB_bam_H_SSEP1[16,]))
GR_high_SEP
first_wave_high_SEP<-as.vector(as.matrix(tab_m_PTB_bam_H_SSEP1[17,]))
first_wave_high_SEP
second_wave_high_SEP<-as.vector(as.matrix(tab_m_PTB_bam_H_SSEP1[18,]))
second_wave_high_SEP

estimates_crises_SEP <- data.frame(rbind(HW_low_SEP, HW_medium_SEP, HW_high_SEP, GR_low_SEP, GR_medium_SEP, GR_high_SEP, 
                                         first_wave_low_SEP, first_wave_medium_SEP, first_wave_high_SEP,
                                         second_wave_low_SEP, second_wave_medium_SEP, second_wave_high_SEP))


estimates_crises_SEP['SEP'] <- c("low", "medium", "high", "low", "medium", "high", "low", "medium", "high", "low", "medium", "high")
estimates_crises_SEP['Crises'] <- c("Heatwave", " ", " ", "Great Recession", " ", " ",
                                    "Covid: first wave", " ", " ", "Covid: second wave", " ", " ")

estimates_crises_SEP<-estimates_crises_SEP[, c("Crises", "SEP", "X1","X2", "X3", "X4")]
estimates_crises_SEP <- estimates_crises_SEP %>%
  setNames(c("Crises","SEP", "OR", "lci", "uci", "d"))


HI <- setDT(estimates_crises_SEP, keep.rownames = FALSE)

estimates_crises_SEP_gt <- HI %>%
  gt()%>%
  tab_header(title="Preterm birth risk, stratified by SEP") %>%
  tab_spanner(label = "95% CI", columns = c(lci, uci))

  