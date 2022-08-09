##Inclusion steps
# Data from 2007 only: made as a 1st step with R file "bevn_from_2007.R" to exclude absent GA in the previous years

# Births taking place inside of Switzerland
table(bevn_eco$country_of_birth_cat1, useNA = "always")
# removal of 124074  (outside of Switz) and 1 (NA) entries

bevn_eco_in0 <- bevn_eco %>%
  filter(country_of_birth_cat1=="Switzerland")
table(bevn_eco_in0$country_of_birth_cat1)

dim(bevn_eco)-dim(bevn_eco_in0)
dim(bevn_eco_in0)
#removal of 124075 entries. new nb entries: 1202203

#GA missing
  table(bevn_eco$GA_weeks, useNA = "always")
  #removal of 138581 entries with missing GA
  bevn_eco_in1 <- bevn_eco_in0 %>%
  filter(!is.na(GA_weeks))
  dim(bevn_eco_in0)-dim(bevn_eco_in1)
  dim(bevn_eco_in1)
  table(bevn_eco_in1$GA_weeks, useNA = "always")
  
#removal of 14518 entries. new nb of entries: 1187685

  
#Place of residence of the mother: exclude mothers domiciled abroad / resident_status
+  round(prop.table(table(bevn_eco$resident_status, useNA="always"))*100,2)
  #we will exclude 158606 (status resident=9, domiciled abroad or status= 2 or 4 = non permanent resident or short stay) and no NA
  
   bevn_eco_in2 <- bevn_eco_in1 %>%
    filter(resident_status==1)
   table(bevn_eco_in2$resident_status)
   
   dim(bevn_eco_in1)-dim(bevn_eco_in2)
   dim(bevn_eco_in2)
   #Exclusion of 37160  additional entries. New nb entries: 1150525 
   
   
# Singletons only
   table(bevn_eco$singleton_or_multiple, useNA = "always")
   round(prop.table(table(bevn_eco$singleton_or_multiple, useNA="always"))*100,2)
  #we will exclude 48139 multiple births (2+) and no NA
   
   bevn_eco_in3 <- bevn_eco_in2 %>%
     filter(singleton_or_multiple=="singleton")
   table(bevn_eco_in3$singleton_or_multiple)
   
   dim(bevn_eco_in2)-dim(bevn_eco_in3)
   dim(bevn_eco_in3)
   #Exclusion of 41426 additional entries. New nb entries: 1109099
   

# Exclude entries with BW<500 and GA<22 weeks, exclude missing
   table(bevn_eco$BW_cat2, useNA="always")
   table(bevn_eco$GA_weeks_cat2, useNA="always")
   
   #we will exclude 752  entries with GA<22 weeks, 2407 with BW < 500g (13+14497=14510)  NA
 
  #check if missing birthweights are for births outside of Switz
   table(bevn_eco$BW_cat2, bevn_eco$country_of_birth_cat1, useNA = "always")
   round(prop.table(table(bevn_eco$BW_cat2, bevn_eco$country_of_birth_cat1, useNA="always"), margin=1)*100,2)
   # (124062) 99.78% of missing BW happened outside of Switz, 0.22%(269) happened inside Switzerland
   
   bevn_eco_in4 <- bevn_eco_in3 %>%
     filter(GA_weeks >22 & BW > 500) %>%
     filter(!is.na(BW)) 

   dim(bevn_eco_in3)-dim(bevn_eco_in4)
   dim(bevn_eco_in4)
   #Exclusion of 2180 additional entries. New nb entries: 1106919       
   
# BL between 20 and 65cm only, exclude missing 
   table(bevn_eco$BL_cat, useNA="always")
   # We will exclude 124295  NAs and 177 BL<20cm, and no BL>60cm
   
   #check if missing BL are for births outside of Switz
   table(bevn_eco$BL_cat, bevn_eco$country_of_birth_cat1, useNA = "always")
   round(prop.table(table(bevn_eco$BL_cat, bevn_eco$country_of_birth_cat1, useNA="always"), margin=1)*100,2)
   # (124070) 99.8% of missing BW happened outside of Switz, (225) 0.18% happened inside Switzerland 
   
   bevn_eco_in5 <- bevn_eco_in4 %>%
     filter(BL <= 65 & BL >= 20) %>%
     filter(!is.na(BL))
   table(bevn_eco_in5$BL, useNA="always")
   
   dim(bevn_eco_in4)-dim(bevn_eco_in5)
   dim(bevn_eco_in5)
   #Exclusion of 128 additional entries. New nb entries: 1106679            
   
# Maternal age: exclude missing and exclude age > 50 (to discuss w/ Kaspar)
   table(bevn_eco$mat_age, useNA="always")
   #no missing and 225 >50yo
   bevn_eco_in6 <- bevn_eco_in5 %>%
    filter(!is.na(mat_age)) %>%
   filter(mat_age<51)
   
   dim(bevn_eco_in5)-dim(bevn_eco_in6)
   dim(bevn_eco_in6)
   #Exclusion of 112 additional entries and no missing NA
   
# Live births only
   table(bevn_eco$stillbirth, useNA="always")
   # We will exclude 5218  stillborn and no NAs
   
   bevn_eco_in7 <- bevn_eco_in6 %>%
     filter(stillbirth==0)
   table(bevn_eco_in7$stillbirth, useNA="always")
   
  dim(bevn_eco_in6)-dim(bevn_eco_in7)
   dim(bevn_eco_in7)
  #Exclusion of 3327  additional entries. New nb entries: 1103352