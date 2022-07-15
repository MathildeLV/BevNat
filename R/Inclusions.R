##Inclusion steps

# Births taking place inside of Switzerland
  table(bevn_eco$Geburtsstaat_cat1, useNA = "always")
  round(prop.table(table(bevn_eco$Geburtsstaat_cat1, useNA="always"))*100,2)
  # removal of 148335  (outside of Switz) and 5 (NA) entries
  
  bevn_eco_in1 <- bevn_eco %>%
  filter(Geburtsstaat_cat1=="Switzerland")
  table(bevn_eco_in1$Geburtsstaat_cat1)
  
  dim(bevn_eco)-dim(bevn_eco_in1)
  dim(bevn_eco_in1)
  #removal of 148340 entries. new nb entries: 2824645
  
#Place of residence of the mother: exclude mothers domiciled abroad / Mutter..stÃ¤ndig.oder.nicht.stÃ¤ndiger.Wohnsitz
  table(bevn_eco$Mutter..ständig.oder.nicht.ständiger.Wohnsitz, useNA = "always")
  round(prop.table(table(bevn_eco$Mutter..ständig.oder.nicht.ständiger.Wohnsitz, useNA="always"))*100,2)
  #we will exclude 183076  (status resident=9, domiciled abroad) and no NA
  
   bevn_eco_in2 <- bevn_eco_in1 %>%
    filter(Mutter..ständig.oder.nicht.ständiger.Wohnsitz!=9)
   table(bevn_eco_in2$Mutter..ständig.oder.nicht.ständiger.Wohnsitz)
   
   dim(bevn_eco_in1)-dim(bevn_eco_in2)
   dim(bevn_eco_in2)
   #Exclusion of 41976  additional entries. New nb entries: 2782669 
   
   
# Singletons only
   table(bevn_eco$singleton_or_multiple, useNA = "always")
   round(prop.table(table(bevn_eco$singleton_or_multiple, useNA="always"))*100,2)
  #we will exclude 92864 multiple births (2+) and no NA
   
   bevn_eco_in3 <- bevn_eco_in2 %>%
     filter(singleton_or_multiple=="singleton")
   table(bevn_eco_in3$singleton_or_multiple)
   
   dim(bevn_eco_in2)-dim(bevn_eco_in3)
   dim(bevn_eco_in3)
   #Exclusion of 85462 additional entries. New nb entries: 2697207       
   

# BW between 500g and 8000g only, exclude missing
   table(bevn_eco$BW_cat2, useNA="always")
   #we will exclude 3203 and 151477  NA
 
  #check if missing birthweights are for births outside of Switz
   table(bevn_eco$BW_cat2, bevn_eco$Geburtsstaat_cat1, useNA = "always")
   round(prop.table(table(bevn_eco$BW_cat2, bevn_eco$Geburtsstaat_cat1, useNA="always"), margin=1)*100,2)
   # (148116) 97.78% of missing BW happened outside of Switz, (3360) 2.22% happened inside Switzerland
   
   bevn_eco_in4 <- bevn_eco_in3 %>%
     filter(Kind..Gewicht.in.Gramm <= 8000 & Kind..Gewicht.in.Gramm >= 500) %>%
      filter(!is.na(Kind..Gewicht.in.Gramm))
   table(bevn_eco_in4$BW_cat2, useNA="always")
   
   dim(bevn_eco_in3)-dim(bevn_eco_in4)
   dim(bevn_eco_in4)
   #Exclusion of 5558  additional entries. New nb entries: 2691649       
   
# BL between 20 and 65cm only, exclude missing 
   table(bevn_eco$BL_cat, useNA="always")
   # We will exclude 151516  NAs and 189 BL<20cm
   
   #check if missing BL are for births outside of Switz
   table(bevn_eco$BL_cat, bevn_eco$Geburtsstaat_cat1, useNA = "always")
   round(prop.table(table(bevn_eco$BL_cat, bevn_eco$Geburtsstaat_cat1, useNA="always"), margin=1)*100,2)
   # (148142) 98% of missing BW happened outside of Switz, (3373) 2.23% happened inside Switzerland 
   
   bevn_eco_in5 <- bevn_eco_in4 %>%
     filter(Kind..Grösse.in.Zentimeter <= 65 & Kind..Grösse.in.Zentimeter >= 20) %>%
     filter(!is.na(Kind..Grösse.in.Zentimeter))
   table(bevn_eco_in5$Kind..Grösse.in.Zentimeter, useNA="always")
   
   dim(bevn_eco_in4)-dim(bevn_eco_in5)
   dim(bevn_eco_in5)
   #Exclusion of 789 additional entries. New nb entries: 2690860            
   
# Missing maternal age
   table(bevn_eco$Mutter..Alter.in.erfüllten.Jahren, useNA="always")
   #no missing
   bevn_eco_in6 <- bevn_eco_in5 %>%
    filter(!is.na(Mutter..Alter.in.erfüllten.Jahren))
   dim(bevn_eco_in5)-dim(bevn_eco_in6)
   dim(bevn_eco_in6)
   
   
# Live births only
   table(bevn_eco$lebend.geboren.oder.nicht, useNA="always")
   # We will exclude 11747  stillborn and no NAs
   
   bevn_eco_in7 <- bevn_eco_in6 %>%
     filter(lebend.geboren.oder.nicht==1)
   table(bevn_eco_in7$lebend.geboren.oder.nicht, useNA="always")
   
  dim(bevn_eco_in6)-dim(bevn_eco_in7)
   dim(bevn_eco_in7)
  #Exclusion of 9016 additional entries. New nb entries: 2681844

# Live births only and GA present only (year >2006 and not missing)   
   table(bevn_eco$GA_weeks_cat, useNA="always")
   table(bevn_eco$GA_weeks_cat, bevn_eco$Ereignisjahr, useNA = "always")
   #We will exclude 9 GA before 2006 and 1780447 NAs
   bevn_eco_in8 <- bevn_eco_in7 %>%
     filter(!is.na(GA_weeks_cat)) %>%
     filter(Ereignisjahr>2005)
   table(bevn_eco_in8$GA_weeks_cat, useNA="always")
   
   dim(bevn_eco_in7)-dim(bevn_eco_in8)
   dim(bevn_eco_in8)
   #Exclusion of  additional entries. New nb entries: 
   
   
# GA: births only from 2006 and GA not missing BUT KEEPING stillbirths
   #We will exclude 9 GA before 2006 and 1780447 NAs
   bevn_eco_in9 <- bevn_eco_in6 %>%
     filter(!is.na(GA_weeks_cat)) %>%
    filter(Ereignisjahr>2005)
   table(bevn_eco_in9$GA_weeks_cat, useNA="always")
   
   dim(bevn_eco_in6)-dim(bevn_eco_in9)
   dim(bevn_eco_in9)