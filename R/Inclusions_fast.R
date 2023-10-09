##Inclusion steps
# Data from 2007 only: made as a 1st step with R file "bevn_from_2007.R" to exclude absent GA in the previous years
bevn_eco_in6 <- bevn_eco %>%
  # Births taking place inside of Switzerland
  filter(country_of_birth_cat1=="Switzerland") %>% 
  #GA missing
  filter(!is.na(GA_weeks)) %>% 
  #Place of residence of the mother: exclude mothers domiciled abroad / resident_status
  filter(resident_status==1)%>% 
  # Singletons only
  filter(singleton_or_multiple=="singleton") %>% 
  # Exclude entries with BW<500 OR GA<22 weeks, exclude missing
  filter(GA_weeks >=22 | BW >= 500) %>%
  filter(!is.na(BW)) %>% 
  # BL between 20 and 65cm only, exclude missing 
  filter(BL < 65 & BL >= 20) %>%
  filter(!is.na(BL))%>% 
  # Outliers exclusion : BW <100g or BW >7.5kg. 
  # and BW >2000 & GA<23, or BW<500 & GA>35
  filter(BW >= 100 & BW < 7500) %>%
  filter(!((BW>2000 & GA_weeks<23)|(BW<500 & GA_weeks>35)))%>% 
  # Maternal age: exclude missing and exclude age > 50 (to discuss w/ Kaspar)
  filter(!is.na(mat_age)) %>%
  filter(mat_age<51)
  
### I add here the relativ variables for exposure to the crises (did not work
# before as there were missing cases of GA)
bevn_eco_in6 <- bevn_eco_in6 %>%
 mutate(covid_hosp_1st_trim_relativ=normalit(covid_hosp_1st_trim),
        covid_hosp_last_trim_relativ=normalit(covid_hosp_last_trim),
        covid_hosp_relativ_by_pregn_month=normalit(covid_hosp_by_pregn_month),
        covid_hosp_relativ_total=normalit(total_covid_hosp),
        # covid_hosp_relativ_total_cat=cut(covid_hosp_relativ_total, breaks=c(0,0.001,0.5,1), include.lowest = TRUE),
        GR_relativ_total=normalit(GR_number_pregn_months),
        GR_relativ_by_pregn_month=normalit(GR_by_pregnancy_months),
        GR_first_trimester_relativ=normalit(GR_first_trimester_continuous),
        GR_last_trimester_relativ=normalit(GR_last_trimester_continuous),
        flu_relativ_total=normalit(flu_number_pregn_months),
        flu_relativ_by_pregn_month=normalit(flu_by_pregnancy_months),
        flu_first_trimester_relativ=normalit(flu_first_trimester_continuous),
        flu_last_trimester_relativ=normalit(flu_last_trimester_continuous)
 )

#Checking
test <- bevn_eco_in6 %>%
  select("month_1", "month_2", "month_3", "month_4",
         "month_5", "month_6", "month_7", "month_8",
         "month_9", "month_10", "month_11",
         "birth_Y_M_1stday", "GA_month", 
         "GR_number_pregn_months", "GR_by_pregnancy_months", "GR_by_pregnancy_months_cat2", "GR_relativ_by_pregn_month","GR_relativ_total",
         "flu_number_pregn_months", "flu_by_pregnancy_months", "flu_by_pregnancy_months_cat2", "flu_relativ_by_pregn_month","flu_relativ_total",
         "total_covid_hosp", "covid_hosp_by_pregn_month",
         "covid_hosp_relativ_by_pregn_month", "covid_hosp_by_pregn_month_cat2", "covid_hosp_relativ_total" )

# Live births only
    bevn_eco_in7 <- bevn_eco_in6 %>%
     filter(stillbirth==0)
   table(bevn_eco_in7$stillbirth, useNA="always")
     

## Dataset 6 and 7 with different maternal nationality categories
   table(bevn_eco_in7$mother_nationality_cat2)
   ### only Swiss mothers
   bevn_eco_in6_Swiss <- bevn_eco_in6 %>%
     filter(mother_nationality_cat2=="Switzerland")
   bevn_eco_in7_Swiss <- bevn_eco_in7 %>%
     filter(mother_nationality_cat2=="Switzerland")
   ### only African
   bevn_eco_in6_Afr <- bevn_eco_in6 %>%
     filter(mother_nationality_cat2=="Africa")
   bevn_eco_in7_Afr <- bevn_eco_in7 %>%
     filter(mother_nationality_cat2=="Africa")
   ### only Asian
   bevn_eco_in6_Asi <- bevn_eco_in6 %>%
     filter(mother_nationality_cat2=="Asia")
   bevn_eco_in7_Asi <- bevn_eco_in7 %>%
     filter(mother_nationality_cat2=="Asia")
   ### only Europe (excl. Switz)
   bevn_eco_in6_Eur <- bevn_eco_in6 %>%
     filter(mother_nationality_cat2=="Europe")
   bevn_eco_in7_Eur <- bevn_eco_in7 %>%
     filter(mother_nationality_cat2=="Europe")
   ### only North America
   bevn_eco_in6_Nort_Am <- bevn_eco_in6 %>%
     filter(mother_nationality_cat2=="Northern America")
   bevn_eco_in7_Nort_Am <- bevn_eco_in7 %>%
     filter(mother_nationality_cat2=="Northern America")
   ### only Southern and Central America
   bevn_eco_in6_S_C_Am <- bevn_eco_in6 %>%
     filter(mother_nationality_cat2=="Southern and Central America")
   bevn_eco_in7_S_C_Am <- bevn_eco_in7 %>%
     filter(mother_nationality_cat2=="Southern and Central America")

## Dataset 6 and 7 with different SSEP categories
   ## mean_SSEP_cat1: tertiles
   ### only Low SSEP
   bevn_eco_in6_L_SSEP1 <- bevn_eco_in6 %>%
     filter(mean_ssep2_cat1=="low SSEP")
   table(bevn_eco_in6_L_SSEP1$mean_ssep2_cat1, useNA = "always")
   bevn_eco_in7_L_SSEP1 <- bevn_eco_in7 %>%
     filter(mean_ssep2_cat1=="low SSEP")
   table(bevn_eco_in7_L_SSEP1$mean_ssep2_cat1, useNA = "always")
   ### only medium SSEP
   bevn_eco_in6_M_SSEP1 <- bevn_eco_in6 %>%
     filter(mean_ssep2_cat1=="medium SSEP")
   table(bevn_eco_in6_M_SSEP1$mean_ssep2_cat1, useNA = "always")
   bevn_eco_in7_M_SSEP1 <- bevn_eco_in7 %>%
     filter(mean_ssep2_cat1=="medium SSEP")
   table(bevn_eco_in7_M_SSEP1$mean_ssep2_cat1, useNA = "always")
   ### only high SSEP
   bevn_eco_in6_H_SSEP1 <- bevn_eco_in6 %>%
     filter(mean_ssep2_cat1=="high SSEP")
   table(bevn_eco_in6_H_SSEP1$mean_ssep2_cat1, useNA = "always")
   bevn_eco_in7_H_SSEP1 <- bevn_eco_in7 %>%
     filter(mean_ssep2_cat1=="high SSEP")
   table(bevn_eco_in7_H_SSEP1$mean_ssep2_cat1, useNA = "always")
   

## Dataset 6 and 7 with different Language regions
   bevn_eco_in6_German <- bevn_eco_in6 %>%
     filter(Language=="German or Romansh")
   bevn_eco_in7_German <- bevn_eco_in7 %>%
     filter(Language=="German or Romansh")
   table(bevn_eco_in7_German$Language, useNA = "always")
   ### only French language region
   bevn_eco_in6_French <- bevn_eco_in6 %>%
     filter(Language=="French")
   bevn_eco_in7_French <- bevn_eco_in7 %>%
     filter(Language=="French")
   table(bevn_eco_in7_French$Language, useNA = "always")
   ### only Italian language region
   bevn_eco_in6_Italian <- bevn_eco_in6 %>%
     filter(Language=="Italian")
   bevn_eco_in7_Italian <- bevn_eco_in7 %>%
     filter(Language=="Italian")
   table(bevn_eco_in7_Italian$Language, useNA = "always")
   
## Dataset 6 and 7 by sex
   bevn_eco_in6_females <- bevn_eco_in6 %>%
     filter(sex=="2")
   bevn_eco_in6_males <- bevn_eco_in6 %>%
     filter(sex=="1")
   bevn_eco_in7_females <- bevn_eco_in7 %>%
     filter(sex=="2")
   bevn_eco_in7_males <- bevn_eco_in7 %>%
     filter(sex=="1")
   
## for birthrate: births from permanent residents only
   bevn_eco_in_br <- bevn_eco %>%
     filter(resident_status==1)
   table(bevn_eco_in_br$resident_status)
   
   dim(bevn_eco)-dim(bevn_eco_in_br)
   dim(bevn_eco_in_br)
   
# # Dataset 6 and 7 with Basel-stadt only
#    bevn_eco_in6_Basel <- bevn_eco_in6 %>%
#      filter(com==2701 | com==2702 | com==2703)
#    bevn_eco_in7_Basel <- bevn_eco_in7 %>%
#      filter(com==2701 | com==2702 | com==2703)
#    bevn_eco_in7_Baselbelow35 <- bevn_eco_in7_Basel %>%
#      filter(mat_age<35)
#    bevn_eco_in7_Baselabove35 <- bevn_eco_in7_Basel %>%
#      filter(mat_age>35 | mat_age==35)
   
   
# Dataset 7 with first parities only
   #global dataset (remark: parity not available for stillbirth cases, so we only do the sensitivity analysis with BW and PTB outcome variables)
   bevn_eco_in7_primiparous <- bevn_eco_in7 %>%
     filter(parity==1)
   # bevn_eco_in7_multiparous <- bevn_eco_in7 %>%
   #   filter(parity>1)
   ## Primiparous X different maternal nationality categories
   # table(bevn_eco_in7_primiparous$mother_nationality_cat2)
   #  bevn_eco_in7_primiparous_Swiss <- bevn_eco_in7_primiparous %>%
   #   filter(mother_nationality_cat2=="Switzerland")
   # bevn_eco_in7_primiparous_Afr <- bevn_eco_in7_primiparous %>%
   #   filter(mother_nationality_cat2=="Africa")
   #  bevn_eco_in7_primiparous_Asi <- bevn_eco_in7_primiparous %>%
   #   filter(mother_nationality_cat2=="Asia")
   # bevn_eco_in7_primiparous_Eur <- bevn_eco_in7_primiparous %>%
   #   filter(mother_nationality_cat2=="Europe")
   #  bevn_eco_in7_primiparous_Nort_Am <- bevn_eco_in7_primiparous %>%
   #   filter(mother_nationality_cat2=="Northern America")
   # bevn_eco_in7_primiparous_S_C_Am <- bevn_eco_in7_primiparous %>%
   #   filter(mother_nationality_cat2=="Southern and Central America")
   # 
   # ## Primiparous X different SSEP (tertiles)
   # table(bevn_eco_in7_primiparous$mean_ssep2_cat1, useNA = "always")
   #  bevn_eco_in7_primiparous_L_SSEP1 <- bevn_eco_in7_primiparous %>%
   #   filter(mean_ssep2_cat1=="low SSEP")
   # table(bevn_eco_in7_primiparous_L_SSEP1$mean_ssep2_cat1, useNA = "always")
   # bevn_eco_in7_primiparous_M_SSEP1 <- bevn_eco_in7_primiparous %>%
   #   filter(mean_ssep2_cat1=="medium SSEP")
   # table(bevn_eco_in7_primiparous_M_SSEP1$mean_ssep2_cat1, useNA = "always")
   # bevn_eco_in7_primiparous_H_SSEP1 <- bevn_eco_in7_primiparous %>%
   #   filter(mean_ssep2_cat1=="high SSEP")
   # table(bevn_eco_in7_primiparous_H_SSEP1$mean_ssep2_cat1, useNA = "always")
   # 
   # ## Primiparous X different language regions
   # bevn_eco_in7_primiparous_German <- bevn_eco_in7_primiparous %>%
   #   filter(Language=="German or Romansh")
   #  bevn_eco_in7_primiparous_French <- bevn_eco_in7_primiparous %>%
   #   filter(Language=="French")
   # bevn_eco_in7_primiparous_Italian <- bevn_eco_in7_primiparous %>%
   #   filter(Language=="Italian")

   # ## Canton of Berne 2011-2021 (for Vivienne Paper)
   # bevn_eco_in7_Berne <- bevn_eco_in7 %>%
   # filter(grepl('BE', Medstat)) %>%
   #   filter(birthyear>2010)
   # 

   