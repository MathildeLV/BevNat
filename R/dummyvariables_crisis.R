#Heatwaves: Canicule européenne de 2015 29/06 until 08/07
#2018 : aout
#2019 Canicule européenne de 2019 23/06 – 01/08 (not included here) 

########################################## Heatwaves ###############################################################
bevn_eco <- bevn_eco %>%
  dplyr::mutate(HW=ifelse((birthyear==2015 & birthmonth==7) 
                          | (birthyear==2018 & birthmonth==8)
                          , '1',
                             0),
                HW=as.factor(HW))
table(bevn_eco$birthyear, bevn_eco$HW)
table(bevn_eco$birthmonth, bevn_eco$HW)
table(bevn_eco$birthmonth, bevn_eco$birthyear, bevn_eco$HW)


# Exposure during one of the pregnancy trimesters
  ## Exposure during first trimester (heatwave 2015 or 2018, independently)
  bevn_eco <- bevn_eco %>%
  dplyr::mutate(HW_first_trimester=ifelse((month_1=="2015-07-01" | month_1=="2018-08-01"| 
                                          month_2=="2015-07-01" |month_2=="2018-08-01"| 
                                          month_3=="2015-07-01" |month_3=="2018-08-01") 
                                             , '1',
                                             0),
                HW_first_trimester=as.factor(HW_first_trimester)) %>%
  dplyr::mutate(HW_first_trimester= replace(HW_first_trimester,
                                               (is.na(HW_first_trimester)), 0))

  ## Exposure during second trimester (heatwave 2015 or 2018, independently)
  bevn_eco <- bevn_eco %>%
  dplyr::mutate(HW_second_trimester=ifelse((month_4=="2015-07-01" | month_4=="2018-08-01"| 
                                              month_5=="2015-07-01" |month_5=="2018-08-01"| 
                                              month_6=="2015-07-01" |month_6=="2018-08-01") 
                                           , '1',
                                           0),
                HW_second_trimester=as.factor(HW_second_trimester)) %>%
  dplyr::mutate(HW_second_trimester= replace(HW_second_trimester,
                                                (is.na(HW_second_trimester)), 0))

  ## Exposure during third trimester (heatwave 2015 or 2018, independently) - up to delivery
  bevn_eco <- bevn_eco %>%
  dplyr::mutate(HW_third_trimester=ifelse((month_7=="2015-07-01" | month_7=="2018-08-01"| 
                                              month_8=="2015-07-01" |month_8=="2018-08-01"| 
                                              month_9=="2015-07-01" |month_9=="2018-08-01" |
                                              month_10=="2015-07-01" |month_10=="2018-08-01"|
                                              month_11=="2015-07-01" |month_11=="2018-08-01") 
                                             , '1',
                                             0),
                HW_third_trimester=as.factor(HW_third_trimester)) %>%
  dplyr::mutate(HW_third_trimester= replace(HW_third_trimester,
                                               (is.na(HW_third_trimester)), 0))

########################################## Economic crises ###############################################################
## Great recession: Q4 2008 and Q1 2009
bevn_eco <- bevn_eco %>%
  dplyr::mutate(GR=ifelse((birthyear==2008 & (birthmonth==10 | birthmonth==11 | birthmonth==12)) 
                          | (birthyear==2009 & (birthmonth==1 | birthmonth==2 | birthmonth==3))
                          , '1',
                          0),
                GR=as.factor(GR))
table(bevn_eco$birthmonth, bevn_eco$birthyear, bevn_eco$GR)

# Exposure during one of the pregnancy trimesters
  ## Exposure during first trimester
  bevn_eco <- bevn_eco %>%
  dplyr::mutate(GR_first_trimester=ifelse((month_1=="2008-10-01" | month_1=="2008-11-01"| month_1=="2008-12-01" |
                                          month_1=="2009-01-01" | month_1=="2009-02-01" | month_1=="2009-03-01" |
                                          month_2=="2008-10-01" | month_2=="2008-11-01"| month_2=="2008-12-01" |
                                          month_2=="2009-01-01" | month_2=="2009-02-01" | month_2=="2009-03-01"|
                                          month_3=="2008-10-01" | month_3=="2008-11-01"| month_3=="2008-12-01" |
                                          month_3=="2009-01-01" | month_3=="2009-02-01" | month_3=="2009-03-01"
                                          ) 
                                          , '1',
                                          0),
                GR_first_trimester=as.factor(GR_first_trimester)) %>%
  dplyr::mutate(GR_first_trimester= replace(GR_first_trimester,
                                            (is.na(GR_first_trimester)), 0))

  ## Exposure during second trimester
  bevn_eco <- bevn_eco %>%
  dplyr::mutate(GR_second_trimester=ifelse((month_4=="2008-10-01" | month_4=="2008-11-01"| month_4=="2008-12-01" |
                                              month_4=="2009-01-01" | month_4=="2009-02-01" | month_4=="2009-03-01" |
                                              month_5=="2008-10-01" | month_5=="2008-11-01"| month_5=="2008-12-01" |
                                              month_5=="2009-01-01" | month_5=="2009-02-01" | month_5=="2009-03-01"|
                                              month_6=="2008-10-01" | month_6=="2008-11-01"| month_6=="2008-12-01" |
                                              month_6=="2009-01-01" | month_6=="2009-02-01" | month_6=="2009-03-01"
                                          ) 
                                           , '1',
                                           0),
                GR_second_trimester=as.factor(GR_second_trimester)) %>%
  dplyr::mutate(GR_second_trimester= replace(GR_second_trimester,
                                             (is.na(GR_second_trimester)), 0))

  ## Exposure during third trimester - up to delivery
  bevn_eco <- bevn_eco %>%
  dplyr::mutate(GR_third_trimester=ifelse((month_7=="2008-10-01" | month_7=="2008-11-01"| month_7=="2008-12-01" |
                                             month_7=="2009-01-01" | month_7=="2009-02-01" | month_7=="2009-03-01" |
                                             month_8=="2008-10-01" | month_8=="2008-11-01"| month_8=="2008-12-01" |
                                             month_8=="2009-01-01" | month_8=="2009-02-01" | month_8=="2009-03-01"|
                                             month_9=="2008-10-01" | month_9=="2008-11-01"| month_9=="2008-12-01" |
                                             month_9=="2009-01-01" | month_9=="2009-02-01" | month_9=="2009-03-01" |
                                             month_10=="2008-10-01" | month_10=="2008-11-01" | month_10=="2008-12-01" |
                                             month_10=="2009-01-01" | month_10=="2009-02-01" | month_10=="2009-03-01" |
                                             month_11=="2008-10-01" | month_11=="2008-11-01" | month_11=="2008-12-01" |
                                             month_11=="2009-01-01" | month_11=="2009-02-01" | month_11=="2009-03-01" 
                                          ) 
                                          , '1',
                                          0),
                GR_third_trimester=as.factor(GR_third_trimester)) %>%
  dplyr::mutate(GR_third_trimester= replace(GR_third_trimester,
                                            (is.na(GR_third_trimester)), 0))

########################################## COVID-19 ###############################################################
# Delivery date during COVID19
  ## Delivery date during Wave 1 
bevn_eco <- bevn_eco %>%
  dplyr::mutate(COVID_first_wave=ifelse((birthyear==2020 & (birthmonth==3 | birthmonth==4))
                          , '1',
                          0),
                COVID_first_wave=as.factor(COVID_first_wave))
table(bevn_eco$birthmonth, bevn_eco$birthyear, bevn_eco$COVID_first_wave)
  ## Delivery date during Wave 2
bevn_eco <- bevn_eco %>%
  dplyr::mutate(COVID_second_wave=ifelse((birthyear==2020 & (birthmonth==10 | birthmonth==11 | birthmonth ==12)) 
                               , '1',
                               0),
                COVID_second_wave=as.factor(COVID_second_wave))
table(bevn_eco$birthmonth, bevn_eco$birthyear, bevn_eco$COVID_second_wave)

# Exposure during one of the pregnancy trimesters
  ## Exposure during first trimester (wave 1 or wave 2, independently)
  bevn_eco <- bevn_eco %>%
  dplyr::mutate(COVID_first_trimester=ifelse((month_1=="2020-03-01" | month_1=="2020-04-01" |month_1=="2020-10-01" | month_1=="2020-11-01" | month_1=="2020-12-01" | 
                                              month_2=="2020-03-01" |month_2=="2020-04-01" | month_2=="2020-10-01" | month_2=="2020-11-01" | month_2=="2020-12-01" | 
                                              month_3=="2020-03-01" |month_3=="2020-04-01" | month_3=="2020-10-01" | month_3=="2020-11-01" | month_3=="2020-12-01") 
                                         , '1',
                                         0),
                COVID_first_trimester=as.factor(COVID_first_trimester)) %>%
  dplyr::mutate(COVID_first_trimester= replace(COVID_first_trimester,
                                                  (is.na(COVID_first_trimester)), 0))
  
  ## Exposure during second trimester (Wave 1 or wave 2, independently)
  bevn_eco <- bevn_eco %>%
  dplyr::mutate(COVID_second_trimester=ifelse((month_4=="2020-03-01" | month_4=="2020-04-01" |month_4=="2020-10-01" | month_1=="2020-11-01" | month_1=="2020-12-01" | 
                                                month_5=="2020-03-01" |month_5=="2020-04-01" | month_5=="2020-10-01" | month_2=="2020-11-01" | month_2=="2020-12-01" | 
                                                month_6=="2020-03-01" |month_6=="2020-04-01" | month_6=="2020-10-01" | month_3=="2020-11-01" | month_3=="2020-12-01")
                                               , '1',
                                              0),
                COVID_second_trimester=as.factor(COVID_second_trimester)) %>%
      dplyr::mutate(COVID_second_trimester= replace(COVID_second_trimester,
                                                    (is.na(COVID_second_trimester)), 0))
    
  ## Exposure during third trimester ((wave 1 or wave 2, independently) - up to delivery
  bevn_eco <- bevn_eco %>%
  dplyr::mutate(COVID_third_trimester=ifelse((month_7=="2020-03-01" |month_7=="2020-04-01" | month_7=="2020-10-01" | month_7=="2020-11-01" | month_7=="2020-12-01" | 
                                              month_8=="2020-03-01" |month_8=="2020-04-01" | month_8=="2020-10-01" | month_8=="2020-11-01" | month_8=="2020-12-01" | 
                                              month_9=="2020-03-01" |month_9=="2020-04-01" | month_9=="2020-10-01" | month_9=="2020-11-01" | month_9=="2020-12-01" |
                                              month_10=="2020-03-01"|month_10=="2020-04-01"| month_10=="2020-10-01"| month_10=="2020-11-01"| month_10=="2020-12-01" |
                                              month_11=="2020-03-01"|month_11=="2020-04-01"| month_11=="2020-10-01"| month_11=="2020-11-01"| month_11=="2020-12-01"
                                              ) 
                                             , '1',
                                             0),
                COVID_third_trimester=as.factor(COVID_third_trimester)) %>%
    dplyr::mutate(COVID_third_trimester= replace(COVID_third_trimester,
                                                  (is.na(COVID_third_trimester)), 0))

  table(bevn_eco$COVID_first_trimester, useNA = "always")
  table(bevn_eco$COVID_second_trimester, useNA = "always")
  table(bevn_eco$COVID_third_trimester, useNA = "always")

# Exposure during two pregnancy trimesters (Remark: it can only be trimesters 1 and 3, because waves 1 and 2 are separate by 6 months)
  bevn_eco <- bevn_eco %>%
    dplyr::mutate(COVID_two_trimesters=ifelse((COVID_first_trimester== "1" & COVID_third_trimester=="1") 
    , '1',
    0),
    COVID_two_trimesters=as.factor(COVID_two_trimesters))
  table(bevn_eco$COVID_two_trimesters, useNA = "always")
  
  # ## Exposure during one trimester, any (first, second, or third, but not two trimesters of exposre) 
  # bevn_eco <- bevn_eco %>%
  #   dplyr::mutate(COVID_one_trimester=ifelse((COVID_first_trimester== "1" | COVID_second_trimester=="1" | COVID_third_trimester=="1") 
  #                                             , '1',
  #                                             0),
  #                 COVID_one_trimester=as.factor(COVID_one_trimester)) %>%
  # dplyr::mutate(COVID_one_trimester= replace(COVID_one_trimester,
  #                                              (is.na(COVID_third_trimester)), 0))
  # table(bevn_eco$COVID_two_trimesters, useNA = "always")