# Renaming most variables
bevn_eco <- bevn_eco %>%
  dplyr::rename(birthyear=Ereignisjahr) %>%
  dplyr::rename(birthmonth=Ereignismonat) %>%
  dplyr::rename(mat_age=Mutter..Alter.in.erfüllten.Jahren) %>%
  dplyr::rename(pat_age=Vater..Alter.in.erfüllten.Jahren) %>%
  dplyr::rename(sex=Kind..Geschlecht) %>%
  dplyr::rename(GA_days=Kind..Gestationsalter.in.Tagen) %>%
  dplyr::rename(country_of_birth=Geburtsstaat) %>%
  dplyr::rename(nb_of_babies=Art.der.Geburt) %>%
  dplyr::rename(BL=Kind..Grösse.in.Zentimeter) %>%
  dplyr::rename(BW=Kind..Gewicht.in.Gramm) %>%
  dplyr::rename(mother_nationality=Mutter..Staatsangehörigkeit) %>%
  dplyr::rename(father_nationality=Vater..Staatsangehörigkeit) %>%
  dplyr::rename(parity=Kind..biologischer.Rang) %>%
  dplyr::rename(resident_status=Mutter..ständig.oder.nicht.ständiger.Wohnsitz)

#recoding variables to NAs 

  #country_of_birth = State and world region of birth
  bevn_eco$country_of_birth[bevn_eco$country_of_birth ==8999] = NA
  ## Mutter: Wohngemeinde / Wohnstaat = Place of residence Mother
  bevn_eco$com[bevn_eco$com ==8999] = NA
  ## Vater: Staatsangehorigkeit = Nationality father
  bevn_eco$father_nationality[bevn_eco$father_nationality ==8999|bevn_eco$father_nationality ==8998] = NA
  ## Kind: Grsse in Zentimeter = length at birth (cm)
  bevn_eco$BL[bevn_eco$BL ==99 | bevn_eco$BL==0] = NA
  ## Kind: Gewicht in Gramm = weight at birth (g)
  bevn_eco$BW[bevn_eco$BW ==9999 |bevn_eco$BW==0] = NA
  bevn_eco$mother_nationality[bevn_eco$mother_nationality ==8999] = NA
  bevn_eco$father_nationality[bevn_eco$father_nationality ==8999] = NA
  

# categorizing state of birth in : Switzerland or Outside of Switzerland
bevn_eco$country_of_birth_cat1 <- cut(bevn_eco$country_of_birth, breaks=c(8000, 8100, 10000), include.lowest=TRUE, labels=c("Switzerland", "Outside Switzerland"))
table(bevn_eco$country_of_birth_cat1, useNA = "always")
bevn_eco$country_of_birth_cat1 <- relevel (bevn_eco$country_of_birth_cat1, ref = "Switzerland")

#categorizing state of birth in : Switzerland, Europe, Africa, Northern America, Southern America, Asia, Oceania, other
bevn_eco$country_of_birth_cat2 <- as.factor(ifelse(bevn_eco$country_of_birth == 8100, 'Switzerland', 
                                                  ifelse((bevn_eco$country_of_birth > 8200 & bevn_eco$country_of_birth < 8281) | (bevn_eco$country_of_birth > 8571 & bevn_eco$country_of_birth < 8575) | (bevn_eco$country_of_birth > 8575 & bevn_eco$country_of_birth < 8579), 'Europe',
                                                  ifelse((bevn_eco$country_of_birth > 8300 & bevn_eco$country_of_birth < 8385) | (bevn_eco$country_of_birth == 8551), 'Africa',
                                                  ifelse((bevn_eco$country_of_birth > 8400 & bevn_eco$country_of_birth < 8404) | (bevn_eco$country_of_birth > 8404 & bevn_eco$country_of_birth < 8413) | (bevn_eco$country_of_birth > 8413 & bevn_eco$country_of_birth < 8422) | (bevn_eco$country_of_birth >8423 & bevn_eco$country_of_birth < 8434) | (bevn_eco$country_of_birth > 8434 & bevn_eco$country_of_birth < 8439) | (bevn_eco$country_of_birth > 8439 & bevn_eco$country_of_birth < 8447) | (bevn_eco$country_of_birth > 8447 & bevn_eco$country_of_birth < 8487), 'Southern and Central America ',
                                                  ifelse(bevn_eco$country_of_birth == 8404 | bevn_eco$country_of_birth == 8413 | bevn_eco$country_of_birth == 8423 | bevn_eco$country_of_birth== 8434 | bevn_eco$country_of_birth== 8439 | bevn_eco$country_of_birth == 8447, 'Northern America',
                                                  ifelse((bevn_eco$country_of_birth > 8500 & bevn_eco$country_of_birth < 8551) | (bevn_eco$country_of_birth >8551 & bevn_eco$country_of_birth < 8572) | bevn_eco$country_of_birth== 8575 | bevn_eco$country_of_birth== 8579, 'Asia',
                                                  ifelse((bevn_eco$country_of_birth > 8600 & bevn_eco$country_of_birth < 8686), 'Oceania', NA
                                                  ))))))))
  table(bevn_eco$country_of_birth_cat2, useNA = "always")
  subset(bevn_eco, is.na(country_of_birth_cat2))
  bevn_eco$country_of_birth_cat2 <- relevel (bevn_eco$country_of_birth_cat2, ref = "Switzerland")
  
#categorizing state of birth in : Switzerland, different parts of Europe, other
bevn_eco$country_of_birth_cat3 <- as.factor(ifelse(bevn_eco$country_of_birth == 8100, 'Switzerland',
                                    ifelse(bevn_eco$country_of_birth == 8207 | bevn_eco$country_of_birth == 8208 | bevn_eco$country_of_birth == 8209 | bevn_eco$country_of_birth == 8222 | bevn_eco$country_of_birth == 8229 | bevn_eco$country_of_birth == 8230 | bevn_eco$country_of_birth == 8238 | bevn_eco$country_of_birth == 8243 | bevn_eco$country_of_birth == 8244,  'Central Europe',
                                    ifelse (bevn_eco$country_of_birth == 8201 | bevn_eco$country_of_birth == 8205 | bevn_eco$country_of_birth ==8220 | bevn_eco$country_of_birth == 8232 | bevn_eco$country_of_birth == 82880 | (bevn_eco$country_of_birth > 8247 & bevn_eco$country_of_birth< 8258) , 'SE Europe',
                                    ifelse(bevn_eco$country_of_birth==8202 | bevn_eco$country_of_birth== 8213 | bevn_eco$country_of_birth==8231 |bevn_eco$country_of_birth== 8236, 'SW Europe', 
                                    ifelse(bevn_eco$country_of_birth==8204 | bevn_eco$country_of_birth==8212 |bevn_eco$country_of_birth==8215|bevn_eco$country_of_birth==8216|bevn_eco$country_of_birth==8223|bevn_eco$country_of_birth==8225|bevn_eco$country_of_birth==8226|bevn_eco$country_of_birth==8227|bevn_eco$country_of_birth==8270|bevn_eco$country_of_birth==8271|bevn_eco$country_of_birth==8272|bevn_eco$country_of_birth==8275, 'Western Europe',
                                    ifelse(bevn_eco$country_of_birth==8206|bevn_eco$country_of_birth==8210|bevn_eco$country_of_birth==8211|bevn_eco$country_of_birth==8217|bevn_eco$country_of_birth==8228|bevn_eco$country_of_birth==8234|bevn_eco$country_of_birth==8273|bevn_eco$country_of_birth==8274,'Northern Europe', 
                                    ifelse(bevn_eco$country_of_birth==8214|bevn_eco$country_of_birth==8218|bevn_eco$country_of_birth==8224|bevn_eco$country_of_birth==8233|bevn_eco$country_of_birth==8239|bevn_eco$country_of_birth==8241|bevn_eco$country_of_birth==8242, 'Southern Europe',
                                    ifelse(bevn_eco$country_of_birth==8235|bevn_eco$country_of_birth==8572|bevn_eco$country_of_birth==8573|bevn_eco$country_of_birth==8574|bevn_eco$country_of_birth==8576|bevn_eco$country_of_birth==8577|bevn_eco$country_of_birth==8578 | (bevn_eco$country_of_birth > 8259 & bevn_eco$country_of_birth < 8267), 'Eastern Europe','other')))))))))
 table(bevn_eco$country_of_birth_cat3, useNA = "always")
 bevn_eco$country_of_birth_cat3 <- relevel (bevn_eco$country_of_birth_cat3, ref = "Switzerland")


## Mutter: Staatsangehörigkeit = Nationality Mother
  #categorizing nationality of the mother in : Switzerland or Outside of Switzerland
  bevn_eco$mother_nationality_cat1 <- cut(bevn_eco$mother_nationality, breaks=c(8000, 8100, 10000), include.lowest=TRUE, labels=c("Switzerland", "Outside Switzerland"))
  bevn_eco$mother_nationality_cat1 <- relevel (bevn_eco$mother_nationality_cat1, ref = "Switzerland")
  
  # Categorizing nationality of the mother in : Switzerland, Europe, Africa, Americas, Asia, Oceania, other
  bevn_eco$mother_nationality_cat2_with_Oc <- as.factor(ifelse(bevn_eco$mother_nationality == 8100, 'Switzerland', 
                                              ifelse((bevn_eco$mother_nationality > 8200 & bevn_eco$mother_nationality < 8281) | (bevn_eco$mother_nationality > 8571 & bevn_eco$mother_nationality < 8575) | (bevn_eco$mother_nationality > 8575 & bevn_eco$mother_nationality < 8579), 'Europe',
                                              ifelse((bevn_eco$mother_nationality > 8300 & bevn_eco$mother_nationality < 8385) | (bevn_eco$mother_nationality == 8551), 'Africa',
                                               ifelse((bevn_eco$mother_nationality > 8400 & bevn_eco$mother_nationality < 8404) | (bevn_eco$mother_nationality > 8404 & bevn_eco$mother_nationality < 8413) | (bevn_eco$mother_nationality > 8413 & bevn_eco$mother_nationality < 8422) | (bevn_eco$mother_nationality >8423 & bevn_eco$mother_nationality < 8434) | (bevn_eco$mother_nationality > 8434 & bevn_eco$mother_nationality < 8439) | (bevn_eco$mother_nationality > 8439 & bevn_eco$mother_nationality < 8447) | (bevn_eco$mother_nationality > 8447 & bevn_eco$mother_nationality < 8487), 'Southern and Central America ',
                                               ifelse(bevn_eco$mother_nationality == 8404 | bevn_eco$mother_nationality == 8413 | bevn_eco$mother_nationality == 8423 | bevn_eco$mother_nationality== 8434 | bevn_eco$mother_nationality== 8439 | bevn_eco$mother_nationality == 8447, 'Northern America',
                                               ifelse((bevn_eco$mother_nationality > 8500 & bevn_eco$mother_nationality < 8551) | (bevn_eco$mother_nationality >8551 & bevn_eco$mother_nationality < 8572) | bevn_eco$mother_nationality== 8575 | bevn_eco$mother_nationality== 8579, 'Asia',
                                               ifelse((bevn_eco$mother_nationality > 8600 & bevn_eco$mother_nationality < 8686), 'Oceania', NA
                                               ))))))))
  table(bevn_eco$mother_nationality_cat2_with_Oc, useNA = "always")
  # too few mothers from Oceania: decided to pool NAs+ mothers from Oceanian nationality
  
  # Categorizing nationality of the mother in : Switzerland, Europe, Africa, Americas, Asia, NA (NA includes NA + Oceania)
  bevn_eco$mother_nationality_cat2 <- as.factor(ifelse(bevn_eco$mother_nationality == 8100, 'Switzerland', 
                                                ifelse((bevn_eco$mother_nationality > 8200 & bevn_eco$mother_nationality < 8281) | (bevn_eco$mother_nationality > 8571 & bevn_eco$mother_nationality < 8575) | (bevn_eco$mother_nationality > 8575 & bevn_eco$mother_nationality < 8579), 'Europe',
                                                ifelse((bevn_eco$mother_nationality > 8300 & bevn_eco$mother_nationality < 8385) | (bevn_eco$mother_nationality == 8551), 'Africa',
                                                ifelse((bevn_eco$mother_nationality > 8400 & bevn_eco$mother_nationality < 8404) | (bevn_eco$mother_nationality > 8404 & bevn_eco$mother_nationality < 8413) | (bevn_eco$mother_nationality > 8413 & bevn_eco$mother_nationality < 8422) | (bevn_eco$mother_nationality >8423 & bevn_eco$mother_nationality < 8434) | (bevn_eco$mother_nationality > 8434 & bevn_eco$mother_nationality < 8439) | (bevn_eco$mother_nationality > 8439 & bevn_eco$mother_nationality < 8447) | (bevn_eco$mother_nationality > 8447 & bevn_eco$mother_nationality < 8487), 'Southern and Central America ',
                                                ifelse(bevn_eco$mother_nationality == 8404 | bevn_eco$mother_nationality == 8413 | bevn_eco$mother_nationality == 8423 | bevn_eco$mother_nationality== 8434 | bevn_eco$mother_nationality== 8439 | bevn_eco$mother_nationality == 8447, 'Northern America',
                                                ifelse((bevn_eco$mother_nationality > 8500 & bevn_eco$mother_nationality < 8551) | (bevn_eco$mother_nationality >8551 & bevn_eco$mother_nationality < 8572) | bevn_eco$mother_nationality== 8575 | bevn_eco$mother_nationality== 8579, 'Asia',
                                                NA)))))))
  bevn_eco$mother_nationality_cat2 <- relevel (bevn_eco$mother_nationality_cat2, ref = "Switzerland")
  table(bevn_eco$mother_nationality_cat2, useNA = "always")
  
  table(bevn_eco$mother_nationality_cat2, bevn_eco$mother_nationality, useNA = "always")
  bevn_eco %>%
  filter(is.na(mother_nationality_cat2)) %>%
  summarise(mother_nationality, mother_nationality_cat2)
  
  #categorizing nationality of the mother in : Switzerland, different parts of Europe, other
  bevn_eco$mother_nationality_cat3 <- as.factor(ifelse(bevn_eco$mother_nationality == 8100, 'Switzerland',
                                            ifelse(bevn_eco$mother_nationality == 8207 | bevn_eco$mother_nationality == 8208 | bevn_eco$mother_nationality == 8209 | bevn_eco$mother_nationality == 8222 | bevn_eco$mother_nationality == 8229 | bevn_eco$mother_nationality == 8230 | bevn_eco$mother_nationality == 8238 | bevn_eco$mother_nationality == 8243 | bevn_eco$mother_nationality == 8244,  'Central Europe',
                                            ifelse (bevn_eco$mother_nationality == 8201 | bevn_eco$mother_nationality == 8205 | bevn_eco$mother_nationality ==8220 | bevn_eco$mother_nationality == 8232 | bevn_eco$mother_nationality == 82880 | (bevn_eco$mother_nationality > 8247 & bevn_eco$mother_nationality< 8258) , 'SE Europe',
                                            ifelse(bevn_eco$mother_nationality==8202 | bevn_eco$mother_nationality== 8213 | bevn_eco$mother_nationality==8231 |bevn_eco$mother_nationality== 8236, 'SW Europe', 
                                            ifelse(bevn_eco$mother_nationality==8204 | bevn_eco$mother_nationality==8212 |bevn_eco$mother_nationality==8215|bevn_eco$mother_nationality==8216|bevn_eco$mother_nationality==8223|bevn_eco$mother_nationality==8225|bevn_eco$mother_nationality==8226|bevn_eco$mother_nationality==8227|bevn_eco$mother_nationality==8270|bevn_eco$mother_nationality==8271|bevn_eco$mother_nationality==8272|bevn_eco$mother_nationality==8275, 'Western Europe',
                                            ifelse(bevn_eco$mother_nationality==8206|bevn_eco$mother_nationality==8210|bevn_eco$mother_nationality==8211|bevn_eco$mother_nationality==8217|bevn_eco$mother_nationality==8228|bevn_eco$mother_nationality==8234|bevn_eco$mother_nationality==8273|bevn_eco$mother_nationality==8274,'Northern Europe', 
                                            ifelse(bevn_eco$mother_nationality==8214|bevn_eco$mother_nationality==8218|bevn_eco$mother_nationality==8224|bevn_eco$mother_nationality==8233|bevn_eco$mother_nationality==8239|bevn_eco$mother_nationality==8241|bevn_eco$mother_nationality==8242, 'Southern Europe',
                                            ifelse(bevn_eco$mother_nationality==8235|bevn_eco$mother_nationality==8572|bevn_eco$mother_nationality==8573|bevn_eco$mother_nationality==8574|bevn_eco$mother_nationality==8576|bevn_eco$mother_nationality==8577|bevn_eco$mother_nationality==8578 | (bevn_eco$mother_nationality > 8259 & bevn_eco$mother_nationality < 8267), 'Eastern Europe','other')))))))))
  table(bevn_eco$mother_nationality_cat3, useNA="always")
  bevn_eco$mother_nationality_cat3 <- relevel (bevn_eco$mother_nationality_cat3, ref = "Switzerland")
  
  
## Mutter: Wohngemeinde / Wohnstaat = Place of residence Mother
  #1-6910 for commune if resident inside Switz <br> 8201-8703 if resident outside of Switz <br>
  # categorizing place of residence of the mother in : inside or outside Switzerland
  bevn_eco$mother_residence_cat1 <- cut(bevn_eco$com, breaks=c(0, 6911, 8704), include.lowest=TRUE, labels=c("Switzerland", "Outside Switzerland"))
  table(bevn_eco$mother_residence_cat1, useNA="always")  
  

  
 ## Categ variable for com (for tables later - it doesnt make sense to group GMDNR like this) 
  bevn_eco$com1 <- cut(bevn_eco$com, breaks=c(1, 1000, 2000, 3000, 4000, 5000, 6000), include.lowest=TRUE)
  
 
  
## Vater: Staatsangehörigkeit = Nationality father
    # categorizing nationality of the father in : inside or outside Switzerland
  bevn_eco$father_nationality_cat1 <- cut(bevn_eco$father_nationality, breaks=c(8000, 8100, 8704), include.lowest=TRUE, labels=c("Switzerland", "Outside Switzerland"))
  
  
## Gestational age
  bevn_eco$GA_days <- as.numeric(bevn_eco$GA_days)
  bevn_eco$GA_weeks <- bevn_eco$GA_days / 7
  
##Birthweight categories  
  ## Kind: Gewicht in Gramm = weight at birth (g)
  #categorize in LBW, "normal" (arbitrary decision) and high BW
  bevn_eco$LBW <- ifelse(bevn_eco$BW<2500, 1, 0)
  bevn_eco$LBW <- as.factor(bevn_eco$LBW)

  
  ## Kind: Gewicht in Gramm = weight at birth (g)
  bevn_eco$BW_cat2 <- cut(bevn_eco$BW, breaks=c(0, 499, 8000, 10000), include.lowest=TRUE)
  table(bevn_eco$BW_cat2, useNA="always")
  
  ## Bw Macrosomia
  bevn_eco$Bw_Macro <- cut(bevn_eco$BW, breaks=c(0, 4000, 10000), include.lowest=TRUE)
  table(bevn_eco$Bw_Macro, useNA="always")
  
  
  ##BW LBW, normal, Macrosomia
  bevn_eco <- bevn_eco %>%
    mutate(BW_cat3char = case_when (BW<2500 & BW>0 ~"Low birthweight",
           BW<4000 & BW>2500 ~"Normal birthweight",
           BW==2500 ~"Normal birthweight",
           BW>4000 | BW==4000~"Macrosomia"
           ))
    table(bevn_eco$BW_cat3char, useNA="always")
  
  bevn_eco <- bevn_eco %>%
    mutate(BW_ordered = case_when (BW<2500 & BW>0 ~"1",
                                   BW<4000 & BW>2500 ~"2",
                                   BW==2500 ~"2",
                                   BW>4000 | BW==4000~"3"
    ),
    BW_ordered=as.numeric(BW_ordered))        
    table(bevn_eco$BW_ordered, useNA="always")
  
  # Low vs normal
  bevn_eco <- bevn_eco %>%
    mutate(LBW_norm = case_when (BW<2500 & BW>0 ~"1",
                                 BW<4000 & BW>2500 | BW ==2500 ~"0"
    ),
    LBW_norm=as.factor(LBW_norm))
  table(bevn_eco$LBW_norm, useNA="always")
  
  # Macrosomia versus normal
  bevn_eco <- bevn_eco %>%
    mutate(macro_norm = case_when (BW>4000 | BW==4000~"1",
                                 BW<4000 & BW>2500 | BW ==2500 ~"0"
    ),
    macro_norm=as.factor(macro_norm))
   table(bevn_eco$macro_norm, useNA="always")
  


  ## BL = BL (cm)
  bevn_eco$BL_cat <- cut(bevn_eco$BL, breaks=c(0, 19, 64, 100), include.lowest=TRUE)
  table(bevn_eco$BL_cat, useNA="always")
  
  
## Art der Geburt = number of babies 
  bevn_eco$nb_of_babies <- as.numeric(bevn_eco$nb_of_babies)
  ##creating categories: single VS multiple pregnancies
  bevn_eco$singleton_or_multiple <- cut(bevn_eco$nb_of_babies, breaks=c(0,1,6), include.lowest = TRUE, labels=c("singleton", "multiple"))
  table(bevn_eco$singleton_or_multiple)
  
  
  
# NEW VARIABLES
  ## Age difference between parents
  bevn_eco$parent_age_diff <- (bevn_eco$pat_age)-(bevn_eco$mat_age)

  ## Creating a variable combining month+year of birth
    #  I created two variables for date : one combining month+year only (birth_Y_M), and another combining day+month+year (birth_Y_M_1stday).
    #  I set up day to 1st of each month in order to have variable m-d-y, easier to work with.
  bevn_eco$birth_Y_M <- with(bevn_eco, sprintf("%d-%02d", birthyear, birthmonth))
  tail(bevn_eco$birth_Y_M)
  head(bevn_eco$birth_Y_M)
  bevn_eco$birth_Y_M <- as.factor(bevn_eco$birth_Y_M)
  bevn_eco$birth_Y_M_num <- as.numeric(bevn_eco$birth_Y_M)
  

  bevn_eco$day <- 01
  bevn_eco$birth_Y_M_1stday <- as.Date(paste(bevn_eco$birthyear, bevn_eco$birthmonth, bevn_eco$day, sep='-'))
  summary(bevn_eco$birth_Y_M_1stday)
  bevn_eco$birth_Y_M_1stday_num <- as.numeric(bevn_eco$birth_Y_M_1stday)
  
  #extract month
  bevn_eco$month <-  format(bevn_eco$birth_Y_M_1stday, "%m")   
  bevn_eco$month <- as.numeric(bevn_eco$month)
  bevn_eco$month1 <- as.factor(bevn_eco$month)
  table(bevn_eco$month)  
  
  #GA category
  bevn_eco <- bevn_eco %>%
    mutate(GA_weeks_cat = cut (GA_weeks, breaks=c(16,24,30,34,39,44,46), include.lowest=TRUE))  
  bevn_eco
    #Second GA category, GA< or >= 22weeks
   bevn_eco <- bevn_eco %>%
  mutate(GA_weeks_cat2=cut(GA_weeks, breaks=c(10,21.9,46), include.lowest=TRUE))
   table(bevn_eco$GA_weeks_cat2)
   
   # Third GA category, GA< or >= 37weeks, PTB
   bevn_eco$PTB <- ifelse(bevn_eco$GA_weeks<37, 1, 0)
   bevn_eco$PTB <- as.factor(bevn_eco$PTB)
   
   # Parity category, 1, 2, 3, 4+
   bevn_eco <- bevn_eco %>%
     dplyr::mutate(parity_cat = cut (parity, breaks=c(0,1,2,3,20)))  
   bevn_eco
   table(bevn_eco$parity_cat, useNA = "always")
   
   # Maternal age category
   table(bevn_eco$Mutter..Alter.in.erreichten.Jahren)
   bevn_eco <- bevn_eco %>%
     dplyr:: mutate(mat_age_cat = cut (Mutter..Alter.in.erreichten.Jahren, breaks=c(10,20,25,30, 35, 40, 70)))  
   
   table(bevn_eco$mat_age_cat, useNA = "always")
   bevn_eco$mat_age_cat <- relevel (bevn_eco$mat_age_cat, ref = 3)
   
   # Paternal age category
   table(bevn_eco$Vater..Alter.in.erreichten.Jahren)
   bevn_eco <- bevn_eco %>%
     dplyr:: mutate(pat_age_cat = cut (Vater..Alter.in.erreichten.Jahren, breaks=c(10,20,30, 40, 50, 70, 100)))  
   
   table(bevn_eco$pat_age_cat, useNA = "always")
   
  #Stillbirth to 0/1 binary variable (1 is stillbirth)
  bevn_eco <- bevn_eco %>%
    dplyr::mutate(lebend.geboren.oder.nicht = as.character(lebend.geboren.oder.nicht),
           stillbirth = dplyr::recode(lebend.geboren.oder.nicht, 
                               "1" = "0",
                               "2" = "1"),
           stillbirth = as.factor(stillbirth))
  

# Chr as factor variables
  #sex of neonate
   # bevn_eco <- bevn_eco %>%
    #  mutate(sex = as.numeric(sex))
    bevn_eco <- bevn_eco %>%
      dplyr::mutate(sex = as.character(sex),
             sex = dplyr::recode(sex, 
                                 "M" = "1",
                                 "F" = "2"),
             sex = as.factor(sex))
    
    #language region
    bevn_eco <- bevn_eco %>%
      mutate(Language=as.character(Language),
             Language=recode(Language, 
                             "1" ="German or Romansh",
                             "2"="French",
                             "3" ="Italian",
                             "4"= "German or Romansh"),
                      Language=as.factor(Language))
    bevn_eco$Language <- relevel (bevn_eco$Language, ref = "German or Romansh")
    
#creating a function to calculate the mode of a variable
    getmode <- function(v) {
      uniqv <- na.omit(unique(v))
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    
    
    
    subset(bevn_eco, birth_Y_M_num==1, c(birth_Y_M_num, birth_Y_M))
    