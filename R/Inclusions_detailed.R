##Inclusion steps
# Data from 2007 only: made as a 1st step with R file "bevn_from_2007.R" to exclude absent GA in the previous years

# Births taking place inside of Switzerland
table(bevn_eco$country_of_birth_cat1, useNA = "always")
# removal of 137194  (outside of Switz) and 1 (NA) entries

bevn_eco_in0 <- bevn_eco %>%
  filter(country_of_birth_cat1=="Switzerland")
table(bevn_eco_in0$country_of_birth_cat1)

dim(bevn_eco)-dim(bevn_eco_in0)
dim(bevn_eco_in0)
100*(dim(bevn_eco)-dim(bevn_eco_in0))/dim(bevn_eco)
#removal of 137194 entries. new nb entries: 1 380 376
# in 9.00%

#GA missing
  table(bevn_eco$GA_weeks, useNA = "always")
  #removal of 151 752 entries with missing GA
  bevn_eco_in1 <- bevn_eco_in0 %>%
  filter(!is.na(GA_weeks))
  dim(bevn_eco_in0)-dim(bevn_eco_in1)
  dim(bevn_eco_in1)
  100*(dim(bevn_eco_in0)-dim(bevn_eco_in1))/dim(bevn_eco_in0)
  
  #removal of 14571 additional entries. new nb of entries: 1 365 805      
  #exclusion of 1.06%
  
#Place of residence of the mother: exclude mothers domiciled abroad / resident_status
  table(bevn_eco$resident_status, useNA="always")
  round(prop.table(table(bevn_eco$resident_status, useNA="always"))*100,2)
  #we will exclude 167 084 (status resident=9 domiciled abroad) + status= 2 (non permanent resident, 9 424) + status=4 (1 014, short stay) and 2 NA
  #total excluded 177 522       
 table(bevn_eco_in1$resident_status, useNA="always")
  
   bevn_eco_in2 <- bevn_eco_in1 %>%
    filter(resident_status==1)
   table(bevn_eco_in2$resident_status)
   
   dim(bevn_eco_in1)-dim(bevn_eco_in2)
   dim(bevn_eco_in2)
   100*(dim(bevn_eco_in1)-dim(bevn_eco_in2))/dim(bevn_eco_in1)
   #Exclusion of 43 400  additional entries, incl. 1 NA. New nb entries: 1 322 405
   #exclusion of 3.18%
   
# Singletons only
   table(bevn_eco$singleton_or_multiple, useNA = "always")
   round(prop.table(table(bevn_eco$singleton_or_multiple, useNA="always"))*100,2)
  #we will exclude 53 917 multiple births (2+) and no NA
   
   bevn_eco_in3 <- bevn_eco_in2 %>%
     filter(singleton_or_multiple=="singleton")
   table(bevn_eco_in3$singleton_or_multiple)
   
   dim(bevn_eco_in2)-dim(bevn_eco_in3)
   dim(bevn_eco_in3)
   100*(dim(bevn_eco_in2)-dim(bevn_eco_in3))/dim(bevn_eco_in2)
   #Exclusion of 46 666 additional entries (=3.53%). New nb entries: 1 275 739
   

# Exclude entries with BW<500 OR GA<22 weeks, exclude missing
   table(bevn_eco$BW_cat2, useNA="always")
   table(bevn_eco$GA_weeks_cat2, useNA="always")
   
   #we will exclude 915 entries with GA<22 weeks, 2 821 with BW < 500g and 137 497 missing BW(NA)
 
  #check if missing birthweights are for births outside of Switz
   table(bevn_eco$BW_cat2, bevn_eco$country_of_birth_cat1, useNA = "always")
   round(prop.table(table(bevn_eco$BW_cat2, bevn_eco$country_of_birth_cat1, useNA="always"), margin=1)*100,2)
   # (137182) 99.77% of missing BW happened outside of Switz, 0.23%(315) happened inside Switzerland
   
   bevn_eco_in4 <- bevn_eco_in3 %>%
     filter(GA_weeks >=22 | BW >= 500) %>%
     filter(!is.na(BW)) 
   # bevn_eco_in4 <- bevn_eco_in3 %>%
   #   filter(GA_weeks >=22) %>%
   #   filter(BW >= 500) %>%
   #   filter(!is.na(BW)) 

   bevn_eco_in4 <- bevn_eco_in3 %>%
     filter(!(GA_weeks <22) | !(BW< 500)) %>%
     filter(!is.na(BW)) 
     
   test <- bevn_eco_in3 %>%
   filter(!is.na(BW)) 
   dim(bevn_eco_in3)-dim(test)
   # 263 missing BW
   
   test2 <- test %>%
     filter(GA_weeks >=22 | BW >= 500)
   dim(test)-dim(test2)
   # 633 GA <22 or BW<500 
   
   dim(bevn_eco_in3)-dim(bevn_eco_in4)
   dim(bevn_eco_in4)
   100*(dim(bevn_eco_in3)-dim(bevn_eco_in4))/dim(bevn_eco_in3)
   
   #Exclusion of 896 additional entries (0.07%). New nb entries: 1 274 843      

   
# BL between 20 and 65cm only, exclude missing 
   table(bevn_eco$BL_cat, useNA="always")
   table(bevn_eco_in4$BL_cat, useNA="always")
   # We will exclude 137 438  NAs and 242 BL<20cm, and no BL>=65cm
   
   #check if missing BL are for births outside of Switz
   table(bevn_eco$BL_cat, bevn_eco$country_of_birth_cat1, useNA = "always")
   round(prop.table(table(bevn_eco$BL_cat, bevn_eco$country_of_birth_cat1, useNA="always"), margin=1)*100,2)
   # (137190) 99.8% of missing BW happened outside of Switz, (248) 0.18% happened inside Switzerland 
   
   bevn_eco_in5 <- bevn_eco_in4 %>%
     filter(BL < 65 & BL >= 20) %>%
     filter(!is.na(BL))
   table(bevn_eco_in5$BL, useNA="always")
   
   dim(bevn_eco_in4)-dim(bevn_eco_in5)
   dim(bevn_eco_in5)
   100*(dim(bevn_eco_in4)-dim(bevn_eco_in5))/dim(bevn_eco_in4)
   #Exclusion of 216 additional entries: 150 NA and 66 outside range (0.02%). New nb entries: 1 274 627                      
   
   
# Outliers xclusion : BW <100g or BW >7.5kg. 
   # and BW >2000 & GA<23, or BW<500 & GA>35
   bevn_eco_in6 <- bevn_eco_in5 %>% 
     filter(BW >= 100 & BW < 7500) %>%
     filter(!((BW>2000 & GA_weeks<23)|(BW<500 & GA_weeks>35)))

   
   dim(bevn_eco_in5)-dim(bevn_eco_in6)
   dim(bevn_eco_in6)
   100*(dim(bevn_eco_in5)-dim(bevn_eco_in6))/dim(bevn_eco_in5)
    #Exclusion of 32 additional entries outside range. New nb entries: 1 274 595                    

# Maternal age: exclude missing and exclude age > 50
   table(bevn_eco$mat_age, useNA="always")
   #no missing and 270 >50yo
   bevn_eco_in6 <- bevn_eco_in6 %>%
    filter(!is.na(mat_age)) %>%
   filter(mat_age<51)
   
   # dim(bevn_eco_in6)-dim(bevn_eco_in6.1)
   # dim(bevn_eco_in6.1)
   # 100*(dim(bevn_eco_in6)-dim(bevn_eco_in6.1))/dim(bevn_eco_in6)
   # #Exclusion of 146 additional entries and no missing NA (0.01%). New nb of entries: 1 274 449       
   
# Livebirths only
   table(bevn_eco$stillbirth, useNA="always")
   # We will exclude 6 005  stillborn and no NAs
   
   bevn_eco_in7 <- bevn_eco_in6 %>%
     filter(stillbirth==0)
   table(bevn_eco_in7$stillbirth, useNA="always")
   
    dim(bevn_eco_in6)-dim(bevn_eco_in7)
    dim(bevn_eco_in7)
    100*(dim(bevn_eco_in6)-dim(bevn_eco_in7))/dim(bevn_eco_in6)
   
  #Exclusion of 4 863  additional entries (0.38%) New nb entries: 1 269 586
    
# ## Dataset to zoom in 2008-2010 (for the 2009 eco crisis)  
#    #stillbirth outcome
#    bevn_eco_in6_2008_10 <- bevn_eco_in6 %>%
#     filter(birthyear>2007 & birthyear <2011)
#    table(bevn_eco_in6_2008_10$birthyear, useNA = "always")
#    
#    #birthweight outcome
#     bevn_eco_in7_2008_10 <- bevn_eco_in7 %>%
#     filter(birthyear>2007 & birthyear <2011)
#    table(bevn_eco_in7_2008_10$birthyear, useNA = "always")
#    
# ## Dataset to zoom in 2017-2020 for covid   
#    #stillbirth outcome
#    bevn_eco_in6_2017_20 <- bevn_eco_in6 %>%
#    filter(birthyear>2016)
#    table(bevn_eco_in6_2017_20$birthyear, useNA = "always")
# 
#    #birthweight outcome
#    bevn_eco_in7_2017_20 <- bevn_eco_in7 %>%
#      filter(birthyear>2016)
#    table(bevn_eco_in7_2017_20$birthyear, useNA = "always")

# ## Heatwaves zooms datasets
#    ### Dataset to zoom in 2014-2016 for the 2015 heatwave
#     #### stillbirth outcome
#     bevn_eco_in6_2014_16 <- bevn_eco_in6 %>%
#      filter(birthyear>2013 & birthyear < 2017)
#    table(bevn_eco_in6_2014_16$birthyear, useNA = "always")
#    
#    #### #birthweight outcome
#    bevn_eco_in7_2014_16 <- bevn_eco_in7 %>%
#      filter(birthyear>2013 & birthyear < 2017)
#    table(bevn_eco_in7_2014_16$birthyear, useNA = "always")   
#    
#    ### Dataset to zoom in 2017-2019 for the 2018 heatwave
#    #### stillbirth outcome
#    bevn_eco_in6_2017_19 <- bevn_eco_in6 %>%
#      filter(birthyear>2016 & birthyear < 2020)
#    table(bevn_eco_in6_2017_19$birthyear, useNA = "always")
#    
#    #### #birthweight outcome
#    bevn_eco_in7_2017_19 <- bevn_eco_in7 %>%
#      filter(birthyear>2016 & birthyear < 2020)
#    table(bevn_eco_in7_2017_19$birthyear, useNA = "always")  
   
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
   
  ## mean_SSEP_cat2: 5th and 95th percentiles
   # ### only Low SSEP
   # bevn_eco_in6_L_SSEP2 <- bevn_eco_in6 %>%
   #   filter(mean_ssep2_cat2=="low SSEP")
   # bevn_eco_in7_L_SSEP2 <- bevn_eco_in7 %>%
   #   filter(mean_ssep2_cat2=="low SSEP")
   # table(bevn_eco_in7_L_SSEP2$mean_ssep2_cat2, useNA = "always")
   # ### only medium SSEP
   # bevn_eco_in6_M_SSEP2 <- bevn_eco_in6 %>%
   #   filter(mean_ssep2_cat2=="medium SSEP")
   # bevn_eco_in7_M_SSEP2 <- bevn_eco_in7 %>%
   #   filter(mean_ssep2_cat2=="medium SSEP")
   # table(bevn_eco_in7_M_SSEP2$mean_ssep2_cat2, useNA = "always")
   # ### only high SSEP
   # bevn_eco_in6_H_SSEP2 <- bevn_eco_in6 %>%
   #   filter(mean_ssep2_cat2=="high SSEP")
   # bevn_eco_in7_H_SSEP2 <- bevn_eco_in7 %>%
   #   filter(mean_ssep2_cat2=="high SSEP")
   # table(bevn_eco_in7_H_SSEP2$mean_ssep2_cat2, useNA = "always")
   
  ## mean_SSEP_cat3: lowest 20% vs. middle (2nd to 4th quintiles) vs. highest 20%. 
   ### only Low SSEP
   # bevn_eco_in6_L_SSEP3 <- bevn_eco_in6 %>%
   #   filter(mean_ssep2_cat3=="low SSEP")
   # bevn_eco_in7_L_SSEP3 <- bevn_eco_in7 %>%
   #   filter(mean_ssep2_cat3=="low SSEP")
   # table(bevn_eco_in7_L_SSEP3$mean_ssep2_cat3, useNA = "always")
   # ### only medium SSEP
   # bevn_eco_in6_M_SSEP3 <- bevn_eco_in6 %>%
   #   filter(mean_ssep2_cat3=="medium SSEP")
   # bevn_eco_in7_M_SSEP3 <- bevn_eco_in7 %>%
   #   filter(mean_ssep2_cat3=="medium SSEP")
   # table(bevn_eco_in7_M_SSEP3$mean_ssep2_cat3, useNA = "always")
   # ### only high SSEP
   # bevn_eco_in6_H_SSEP3 <- bevn_eco_in6 %>%
   #   filter(mean_ssep2_cat3=="high SSEP")
   # bevn_eco_in7_H_SSEP3 <- bevn_eco_in7 %>%
   #   filter(mean_ssep2_cat3=="high SSEP")
   # table(bevn_eco_in7_H_SSEP3$mean_ssep2_cat3, useNA = "always") 
  
   
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
   
   ## Dataset 6 and 7 with different maternal age categories AND ALL parities
   # < 32 yo
   bevn_eco_in6_below32 <- bevn_eco_in6 %>%
     filter(mat_age_cat2==0)
   bevn_eco_in7_below32 <- bevn_eco_in7 %>%
     filter(mat_age_cat2==0)
   ### >= 32 yo
   bevn_eco_in6_above_32 <- bevn_eco_in6 %>%
     filter(mat_age_cat2==1)
   bevn_eco_in7_above_32 <- bevn_eco_in7 %>%
     filter(mat_age_cat2==1)

   ## Dataset 6 and 7 with different maternal age categories AND first parities
   # < 32 yo
   bevn_eco_in6_below32_primiparous <- bevn_eco_in6 %>%
     filter(mat_age_cat2==0) %>%
     filter(parity==1)
   bevn_eco_in7_below32_primiparous <- bevn_eco_in7 %>%
     filter(mat_age_cat2==0) %>%
     filter(parity==1)
   ### >= 32 yo
   bevn_eco_in6_above_32_primiparous <- bevn_eco_in6 %>%
     filter(mat_age_cat2==1) %>%
      filter(parity==1)
   bevn_eco_in7_above_32_primiparous <- bevn_eco_in7 %>%
     filter(mat_age_cat2==1) %>%
     filter(parity==1)
   


# # Dataset 6 and 7 only with COVID-19 exposure, 2015-21
#    bevn_eco_in7_COVID_exp_during_pregnancy <- bevn_eco_in7_15_21 %>%
#      filter(COVID_first_trimester=="1" | COVID_second_trimester=="1" | COVID_third_trimester =="1")
#    table(bevn_eco_in7_COVID_exp_during_pregnancy$COVID_two_trimesters, useNA = "always")
#    bevn_eco_in6_COVID_exp_during_pregnancy <- bevn_eco_in6_15_21 %>%
#      filter(COVID_first_trimester=="1" | COVID_second_trimester=="1" | COVID_third_trimester =="1")
#    table(bevn_eco_in6_COVID_exp_during_pregnancy$COVID_two_trimesters, useNA = "always")
#    
# Dataset 6 and 7 with Basel-stadt only
   # bevn_eco_in6_Basel <- bevn_eco_in6 %>%
   #   filter(com==2701 | com==2702 | com==2703)
   # bevn_eco_in7_Basel <- bevn_eco_in7 %>%
   #   filter(com==2701 | com==2702 | com==2703)
   # bevn_eco_in7_Baselbelow35 <- bevn_eco_in7_Basel %>%
   #   filter(mat_age<35)
   # bevn_eco_in7_Baselabove35 <- bevn_eco_in7_Basel %>%
   #   filter(mat_age>35 | mat_age==35)
   
# dataset 7 with first parities only
   #global dataset (remark: parity not available for stillbirth cases, so we only do the sensitivity analysis with BW and PTB outcome variables)
   bevn_eco_in7_primiparous <- bevn_eco_in7 %>%
     filter(parity==1)
   bevn_eco_in7_multiparous <- bevn_eco_in7 %>%
     filter(parity>1)
   ## Primiparous X different maternal nationality categories
   table(bevn_eco_in7_primiparous$mother_nationality_cat2)
    bevn_eco_in7_primiparous_Swiss <- bevn_eco_in7_primiparous %>%
     filter(mother_nationality_cat2=="Switzerland")
   bevn_eco_in7_primiparous_Afr <- bevn_eco_in7_primiparous %>%
     filter(mother_nationality_cat2=="Africa")
    bevn_eco_in7_primiparous_Asi <- bevn_eco_in7_primiparous %>%
     filter(mother_nationality_cat2=="Asia")
   bevn_eco_in7_primiparous_Eur <- bevn_eco_in7_primiparous %>%
     filter(mother_nationality_cat2=="Europe")
    bevn_eco_in7_primiparous_Nort_Am <- bevn_eco_in7_primiparous %>%
     filter(mother_nationality_cat2=="Northern America")
   bevn_eco_in7_primiparous_S_C_Am <- bevn_eco_in7_primiparous %>%
     filter(mother_nationality_cat2=="Southern and Central America")
  
   ## Primiparous X different SSEP (tertiles)
   table(bevn_eco_in7_primiparous$mean_ssep2_cat1, useNA = "always")
    bevn_eco_in7_primiparous_L_SSEP1 <- bevn_eco_in7_primiparous %>%
     filter(mean_ssep2_cat1=="low SSEP")
   table(bevn_eco_in7_primiparous_L_SSEP1$mean_ssep2_cat1, useNA = "always")
   bevn_eco_in7_primiparous_M_SSEP1 <- bevn_eco_in7_primiparous %>%
     filter(mean_ssep2_cat1=="medium SSEP")
   table(bevn_eco_in7_primiparous_M_SSEP1$mean_ssep2_cat1, useNA = "always")
   bevn_eco_in7_primiparous_H_SSEP1 <- bevn_eco_in7_primiparous %>%
     filter(mean_ssep2_cat1=="high SSEP")
   table(bevn_eco_in7_primiparous_H_SSEP1$mean_ssep2_cat1, useNA = "always")
   
   ## Primiparous X different language regions
   bevn_eco_in7_primiparous_German <- bevn_eco_in7_primiparous %>%
     filter(Language=="German or Romansh")
    bevn_eco_in7_primiparous_French <- bevn_eco_in7_primiparous %>%
     filter(Language=="French")
   bevn_eco_in7_primiparous_Italian <- bevn_eco_in7_primiparous %>%
     filter(Language=="Italian")

   # ## Females only
   # bevn_eco_in7_females <- bevn_eco_in7 %>%
   #   filter(sex==2)
   # bevn_eco_in6_females <- bevn_eco_in6 %>%
   #   filter(sex==2)
   # ## Males only
   # bevn_eco_in7_males <- bevn_eco_in7 %>%
   #   filter(sex==1)
   # bevn_eco_in6_males <- bevn_eco_in6 %>%
   #   filter(sex==1)
   
   ## Canton of Berne 2011-2021 (for Vivienne Paper)
   # bevn_eco_in7_Berne <- bevn_eco_in7 %>%
   # filter(grepl('BE', Medstat)) %>%
   #   filter(birthyear>2010)
   # 
   
   
   ## for birthrate: births from permanent residents only
   bevn_eco_in_br <- bevn_eco %>%
     filter(resident_status==1)
   table(bevn_eco_in_br$resident_status)
   
   dim(bevn_eco)-dim(bevn_eco_in_br)
   dim(bevn_eco_in_br)
