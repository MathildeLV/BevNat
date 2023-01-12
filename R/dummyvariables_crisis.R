#Heatwaves: Canicule européenne de 2015 29/06 until 08/07
#2018 : aout
#2019 Canicule européenne de 2019 23/06 – 01/08

# Heatwaves
bevn_eco <- bevn_eco %>%
  dplyr::mutate(HW=ifelse((birthyear==2015 & birthmonth==7) 
                          | (birthyear==2018 & birthmonth==8)
                          , '1',
                             0),
                HW=as.factor(HW))
table(bevn_eco$birthyear, bevn_eco$HW)
table(bevn_eco$birthmonth, bevn_eco$HW)
table(bevn_eco$birthmonth, bevn_eco$birthyear, bevn_eco$HW)


