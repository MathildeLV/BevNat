#recoding variables to NAs 

  #Geburtsstaat = State and world region of birth
  bevn_eco$Geburtsstaat[bevn_eco$Geburtsstaat ==8999] = NA
  ## Mutter: Wohngemeinde / Wohnstaat = Place of residence Mother
  bevn_eco$com[bevn_eco$com ==8999] = NA
  ## Vater: Staatsangehörigkeit = Nationality father
  bevn_eco$Vater..Staatsangehörigkeit[bevn_eco$Vater..Staatsangehörigkeit ==8999|bevn_eco$Vater..Staatsangehörigkeit ==8998] = NA
  ## Kind: Grösse in Zentimeter = length at birth (cm)
  bevn_eco$Kind..Grösse.in.Zentimeter[bevn_eco$Kind..Grösse.in.Zentimeter ==99 | bevn_eco$Kind..Grösse.in.Zentimeter==0] = NA
  ## Kind: Gewicht in Gramm = weight at birth (g)
  bevn_eco$Kind..Gewicht.in.Gramm[bevn_eco$Kind..Gewicht.in.Gramm ==9999 |bevn_eco$Kind..Gewicht.in.Gramm==0] = NA
  


# categorizing state of birth in : Switzerland or Outside of Switzerland
bevn_eco$Geburtsstaat_cat1 <- cut(bevn_eco$Geburtsstaat, breaks=c(8000, 8100, 10000), include.lowest=TRUE, labels=c("Switzerland", "Outside Switzerland"))
table(bevn_eco$Geburtsstaat_cat1, useNA = "always")
round(prop.table(table(bevn_eco$Geburtsstaat_cat1, useNA="always"))*100,2)

#categorizing state of birth in : Switzerland, Europe, Africa, Americas, Asia, Oceania, other
bevn_eco$Geburtsstaat_cat2 <- cut(bevn_eco$Geburtsstaat, breaks=c(8000, 8100, 8280, 8384, 8486, 8601, 8685, 10000), include.lowest=TRUE, labels=c("Switzerland", "Europe - excl Switz", "Africa", "Americas", "Asia", "Oceania", "Other"))
table(bevn_eco$Geburtsstaat_cat2, useNA = "always")
round(prop.table(table(bevn_eco$Geburtsstaat_cat2, useNA="always"))*100,2)

#categorizing state of birth in : Switzerland, different parts of Europe, other
bevn_eco$Geburtsstaat_cat3 <- as.factor(ifelse(bevn_eco$Geburtsstaat == 8100, 'Switz',
                                    ifelse(bevn_eco$Geburtsstaat == 8207 | bevn_eco$Geburtsstaat == 8208 | bevn_eco$Geburtsstaat == 8209 | bevn_eco$Geburtsstaat == 8222 | bevn_eco$Geburtsstaat == 8229 | bevn_eco$Geburtsstaat == 8230 | bevn_eco$Geburtsstaat == 8238 | bevn_eco$Geburtsstaat == 8243 | bevn_eco$Geburtsstaat == 8244,  'Central Europe',
                                    ifelse (bevn_eco$Geburtsstaat == 8201 | bevn_eco$Geburtsstaat == 8205 | bevn_eco$Geburtsstaat ==8220 | bevn_eco$Geburtsstaat == 8232 | bevn_eco$Geburtsstaat == 82880 | (bevn_eco$Geburtsstaat > 8247 & bevn_eco$Geburtsstaat< 8258) , 'SE Europe',
                                    ifelse(bevn_eco$Geburtsstaat==8202 | bevn_eco$Geburtsstaat== 8213 | bevn_eco$Geburtsstaat==8231 |bevn_eco$Geburtsstaat== 8236, 'SW Europe', 
                                    ifelse(bevn_eco$Geburtsstaat==8204 | bevn_eco$Geburtsstaat==8212 |bevn_eco$Geburtsstaat==8215|bevn_eco$Geburtsstaat==8216|bevn_eco$Geburtsstaat==8223|bevn_eco$Geburtsstaat==8225|bevn_eco$Geburtsstaat==8226|bevn_eco$Geburtsstaat==8227|bevn_eco$Geburtsstaat==8270|bevn_eco$Geburtsstaat==8271|bevn_eco$Geburtsstaat==8272|bevn_eco$Geburtsstaat==8275, 'Western Europe',
                                    ifelse(bevn_eco$Geburtsstaat==8206|bevn_eco$Geburtsstaat==8210|bevn_eco$Geburtsstaat==8211|bevn_eco$Geburtsstaat==8217|bevn_eco$Geburtsstaat==8228|bevn_eco$Geburtsstaat==8234|bevn_eco$Geburtsstaat==8273|bevn_eco$Geburtsstaat==8274,'Northern Europe', 
                                    ifelse(bevn_eco$Geburtsstaat==8214|bevn_eco$Geburtsstaat==8218|bevn_eco$Geburtsstaat==8224|bevn_eco$Geburtsstaat==8233|bevn_eco$Geburtsstaat==8239|bevn_eco$Geburtsstaat==8241|bevn_eco$Geburtsstaat==8242, 'Southern Europe',
                                    ifelse(bevn_eco$Geburtsstaat==8235|bevn_eco$Geburtsstaat==8572|bevn_eco$Geburtsstaat==8573|bevn_eco$Geburtsstaat==8574|bevn_eco$Geburtsstaat==8576|bevn_eco$Geburtsstaat==8577|bevn_eco$Geburtsstaat==8578 | (bevn_eco$Geburtsstaat > 8259 & bevn_eco$Geburtsstaat < 8267), 'Eastern Europe','other')))))))))






## Mutter: Staatsangehörigkeit = Nationality Mother
  #categorizing nationality of the mother in : Switzerland or Outside of Switzerland
  bevn_eco$mother_nationality_cat1 <- cut(bevn_eco$Mutter..Staatsangehörigkeit, breaks=c(8000, 8100, 10000), include.lowest=TRUE, labels=c("Switzerland", "Outside Switzerland"))

  # Categorizing nationality of the mother in : Switzerland, Europe, Africa, Americas, Asia, Oceania, other
  bevn_eco$mother_nationality_cat2 <- cut(bevn_eco$Mutter..Staatsangehörigkeit, breaks=c(8000, 8100, 8280, 8384, 8486, 8601, 8685, 10000), include.lowest=TRUE, labels=c("Switzerland", "Europe - excl Switz", "Africa", "Americas", "Asia", "Oceania", "Other"))
  
  #categorizing nationality of the mother in : Switzerland, different parts of Europe, other
  bevn_eco$mother_nationality_cat3 <- as.factor(ifelse(bevn_eco$Mutter..Staatsangehörigkeit == 8100, 'Switz',
                                            ifelse(bevn_eco$Mutter..Staatsangehörigkeit == 8207 | bevn_eco$Mutter..Staatsangehörigkeit == 8208 | bevn_eco$Mutter..Staatsangehörigkeit == 8209 | bevn_eco$Mutter..Staatsangehörigkeit == 8222 | bevn_eco$Mutter..Staatsangehörigkeit == 8229 | bevn_eco$Mutter..Staatsangehörigkeit == 8230 | bevn_eco$Mutter..Staatsangehörigkeit == 8238 | bevn_eco$Mutter..Staatsangehörigkeit == 8243 | bevn_eco$Mutter..Staatsangehörigkeit == 8244,  'Central Europe',
                                            ifelse (bevn_eco$Mutter..Staatsangehörigkeit == 8201 | bevn_eco$Mutter..Staatsangehörigkeit == 8205 | bevn_eco$Mutter..Staatsangehörigkeit ==8220 | bevn_eco$Mutter..Staatsangehörigkeit == 8232 | bevn_eco$Mutter..Staatsangehörigkeit == 82880 | (bevn_eco$Mutter..Staatsangehörigkeit > 8247 & bevn_eco$Mutter..Staatsangehörigkeit< 8258) , 'SE Europe',
                                            ifelse(bevn_eco$Mutter..Staatsangehörigkeit==8202 | bevn_eco$Mutter..Staatsangehörigkeit== 8213 | bevn_eco$Mutter..Staatsangehörigkeit==8231 |bevn_eco$Mutter..Staatsangehörigkeit== 8236, 'SW Europe', 
                                            ifelse(bevn_eco$Mutter..Staatsangehörigkeit==8204 | bevn_eco$Mutter..Staatsangehörigkeit==8212 |bevn_eco$Mutter..Staatsangehörigkeit==8215|bevn_eco$Mutter..Staatsangehörigkeit==8216|bevn_eco$Mutter..Staatsangehörigkeit==8223|bevn_eco$Mutter..Staatsangehörigkeit==8225|bevn_eco$Mutter..Staatsangehörigkeit==8226|bevn_eco$Mutter..Staatsangehörigkeit==8227|bevn_eco$Mutter..Staatsangehörigkeit==8270|bevn_eco$Mutter..Staatsangehörigkeit==8271|bevn_eco$Mutter..Staatsangehörigkeit==8272|bevn_eco$Mutter..Staatsangehörigkeit==8275, 'Western Europe',
                                            ifelse(bevn_eco$Mutter..Staatsangehörigkeit==8206|bevn_eco$Mutter..Staatsangehörigkeit==8210|bevn_eco$Mutter..Staatsangehörigkeit==8211|bevn_eco$Mutter..Staatsangehörigkeit==8217|bevn_eco$Mutter..Staatsangehörigkeit==8228|bevn_eco$Mutter..Staatsangehörigkeit==8234|bevn_eco$Mutter..Staatsangehörigkeit==8273|bevn_eco$Mutter..Staatsangehörigkeit==8274,'Northern Europe', 
                                            ifelse(bevn_eco$Mutter..Staatsangehörigkeit==8214|bevn_eco$Mutter..Staatsangehörigkeit==8218|bevn_eco$Mutter..Staatsangehörigkeit==8224|bevn_eco$Mutter..Staatsangehörigkeit==8233|bevn_eco$Mutter..Staatsangehörigkeit==8239|bevn_eco$Mutter..Staatsangehörigkeit==8241|bevn_eco$Mutter..Staatsangehörigkeit==8242, 'Southern Europe',
                                            ifelse(bevn_eco$Mutter..Staatsangehörigkeit==8235|bevn_eco$Mutter..Staatsangehörigkeit==8572|bevn_eco$Mutter..Staatsangehörigkeit==8573|bevn_eco$Mutter..Staatsangehörigkeit==8574|bevn_eco$Mutter..Staatsangehörigkeit==8576|bevn_eco$Mutter..Staatsangehörigkeit==8577|bevn_eco$Mutter..Staatsangehörigkeit==8578 | (bevn_eco$Mutter..Staatsangehörigkeit > 8259 & bevn_eco$Mutter..Staatsangehörigkeit < 8267), 'Eastern Europe','other')))))))))
  round(prop.table(table(bevn_eco$mother_nationality_cat3, useNA="always"))*100,2)
  
  
  
  
  
  
## Mutter: Wohngemeinde / Wohnstaat = Place of residence Mother
  #1-6910 for commune if resident inside Switz <br> 8201-8703 if resident outside of Switz <br>
  # categorizing place of residence of the mother in : inside or outside Switzerland
  bevn_eco$mother_residence_cat1 <- cut(bevn_eco$com, breaks=c(0, 6911, 8704), include.lowest=TRUE, labels=c("Switzerland", "Outside Switzerland"))
  table(bevn_eco$mother_residence_cat1, useNA="always")  
  

## Vater: Staatsangehörigkeit = Nationality father
    # categorizing nationality of the father in : inside or outside Switzerland
  bevn_eco$father_nationality_cat1 <- cut(bevn_eco$Vater..Staatsangehörigkeit, breaks=c(8000, 8100, 8704), include.lowest=TRUE, labels=c("Switzerland", "Outside Switzerland"))
  
  
## Gestational age
  bevn_eco$Kind..Gestationsalter.in.Tagen <- as.numeric(bevn_eco$Kind..Gestationsalter.in.Tagen)
  bevn_eco$GA_weeks <- bevn_eco$Kind..Gestationsalter.in.Tagen / 7
  
## Kind: Gewicht in Gramm = weight at birth (g)
  #categorize in LBW, "normal" (arbitrary decision) and high BW
  bevn_eco$BW_cat <- cut(bevn_eco$Kind..Gewicht.in.Gramm, breaks=c(0, 2500, 5000, 10000), include.lowest=TRUE, labels=c("LBW", "NBW", "HBW"))
  table(bevn_eco$BW_cat, useNA="always")
  
  
## Art der Geburt = number of babies 
  bevn_eco$Art.der.Geburt <- as.numeric(bevn_eco$Art.der.Geburt)
  ##creating categories: single VS multiple pregnancies
  bevn_eco$singleton_or_multiple <- cut(bevn_eco$Art.der.Geburt, breaks=c(0,1,6), include.lowest = TRUE, labels=c("singleton", "multiple"))
  table(bevn_eco$singleton_or_multiple)
  
  
  
# NEW VARIABLES
  ## Age difference between parents
  bevn_eco$parent_age_diff <- (bevn_eco$Vater..Alter.in.erfüllten.Jahren)-(bevn_eco$Mutter..Alter.in.erfüllten.Jahren)

  ## Creating a variable combining month+year of birth
    #  I created two variables for date : one combining month+year only, and another combining day+month+year.
    #  I set up day to 1st of each month in order to have variable m-d-y, easier to work with.
  bevn_eco$birthdate3 <- with(bevn_eco, sprintf("%d-%02d", Ereignisjahr, Ereignismonat))
  tail(bevn_eco$birthdate3)
  head(bevn_eco$birthdate3)
  bevn_eco$birthdate3 <- as.factor(bevn_eco$birthdate3)
  
  
  bevn_eco$day <- 01
  bevn_eco$birthdate2 <- as.Date(paste(bevn_eco$Ereignisjahr, bevn_eco$Ereignismonat, bevn_eco$day, sep='-'))
  summary(bevn_eco$birthdate2)

  
  #GA category
  bevn_eco <- bevn_eco %>%
    mutate(GA_weeks_cat = cut (GA_weeks, breaks=c(16,24,30,34,39,44,46), include.lowest=TRUE))  
  bevn_eco