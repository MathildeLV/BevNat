# 1. Exposure during the whole pregnancy
# Heatwave: 0 if the mother was never exposed to heatwave during whole pregnancy,
# 1 if she was exposed during one month (any) - it can only be during one month
# because only 2 heatwaves separated by 3 years.
bevn_eco <- bevn_eco %>%
  dplyr::mutate(HW=ifelse((month_1=="2015-07-01" | month_1=="2018-08-01"|
                          month_2=="2015-07-01" |month_2=="2018-08-01"|
                          month_3=="2015-07-01" |month_3=="2018-08-01" |
                          month_4=="2015-07-01" | month_4=="2018-08-01"|
                          month_5=="2015-07-01" |month_5=="2018-08-01"|
                          month_6=="2015-07-01" |month_6=="2018-08-01" |
                          month_7=="2015-07-01" | month_7=="2018-08-01"|
                          month_8=="2015-07-01" |month_8=="2018-08-01"|
                          month_9=="2015-07-01" |month_9=="2018-08-01" |
                          month_10=="2015-07-01" |month_10=="2018-08-01"|
                          month_11=="2015-07-01" |month_11=="2018-08-01" |
                          birth_Y_M_1stday=="2015-07-01" |  birth_Y_M_1stday=="2018-08-01"),
                          '1',
                          0),
                HW=as.factor(HW)) %>%
  dplyr::mutate(HW= replace(HW, (is.na(HW)), 0))

table(bevn_eco$HW, useNA = "always")
table(bevn_eco$birthyear, bevn_eco$HW)
table(bevn_eco$birthmonth, bevn_eco$birthyear, bevn_eco$HW)

### Economic crisis: 2008-10-01 until 2009-03-01, included.
# Specify columns to count and date condition
columns_to_count <- c("month_1", "month_2", "month_3", "month_4",
                      "month_5", "month_6", "month_7", "month_8",
                      "month_9", "month_10", "month_11",
                      "birth_Y_M_1stday")
condition_start_date <- as.Date("2008-10-01")
condition_end_date <- as.Date("2009-03-01")
# 
# GA_weeks_present <- bevn_eco %>%
#   filter(!is.na(GA_weeks))
# Apply the function to count columns based on the specified conditions for each row
# it counts the number of pregnancy months during which the mother was exposed
# to eco. crisis
bevn_eco$GR_number_pregn_months <- apply(
  bevn_eco[, columns_to_count], 1, count_columns_by_date,
  start_date = condition_start_date, end_date = condition_end_date)

# Influenza 2009: Week 43 (2009) to 1 (2010)
# ie 20-04-2009 - 22-02-2010
condition_start_date <- as.Date("2009-10-01")
condition_end_date <- as.Date("2010-01-01")

# Apply the function to count columns based on the specified conditions for each row
bevn_eco$flu_number_pregn_months <- apply(
  bevn_eco[, columns_to_count], 1, count_columns_by_date,
  start_date = condition_start_date, end_date = condition_end_date)

table(bevn_eco$GR_number_pregn_months, useNA = "always")
table(bevn_eco$flu_number_pregn_months, useNA = "always")

bevn_eco <- bevn_eco %>%
  mutate(GR_by_pregnancy_months=GR_number_pregn_months/GA_month,
         flu_by_pregnancy_months=flu_number_pregn_months/GA_month,
         GR_by_pregnancy_months_cat=cut(GR_by_pregnancy_months, breaks=c(0,0.45, 1.25)),
         GR_by_pregnancy_months_cat2 = case_when (
          GR_by_pregnancy_months_cat=="(0.45,1.25]" ~"highly exposed",
          GR_by_pregnancy_months_cat=="(0,0.45]" ~"moderately exposed",
          GR_by_pregnancy_months==0 ~"never exposed"),
         flu_by_pregnancy_months_cat=cut(flu_by_pregnancy_months, breaks=c(0,0.6, 1.25)),
         flu_by_pregnancy_months_cat2 = case_when (
           flu_by_pregnancy_months_cat=="(0.6,1.25]" ~"highly exposed",
           flu_by_pregnancy_months_cat=="(0,0.6]" ~"moderately exposed",
           flu_by_pregnancy_months==0 ~"never exposed"))
bevn_eco$GR_by_pregnancy_months_cat2 <- factor(bevn_eco$GR_by_pregnancy_months_cat2,
                                                          levels=c('never exposed',
                                                                   'moderately exposed',
                                                                   'highly exposed'))
bevn_eco$flu_by_pregnancy_months_cat2 <- factor(bevn_eco$flu_by_pregnancy_months_cat2,
                                                       levels=c('never exposed',
                                                                'moderately exposed',
                                                                'highly exposed'))
table(bevn_eco$GR_by_pregnancy_months, useNA = "always")
table(bevn_eco$GR_by_pregnancy_months_cat2, useNA = "always")
table(bevn_eco$flu_by_pregnancy_months, useNA = "always")
table(bevn_eco$flu_by_pregnancy_months_cat2, useNA = "always")
table(bevn_eco$covid_hosp_by_pregn_month, useNA = "always")


#Checking
test <- bevn_eco %>%
  select("month_1", "month_2", "month_3", "month_4",
         "month_5", "month_6", "month_7", "month_8",
         "month_9", "month_10", "month_11",
         "birth_Y_M_1stday", "GA_month", 
         "GR_number_pregn_months", "GR_by_pregnancy_months", "GR_by_pregnancy_months_cat2",
         "flu_number_pregn_months", "flu_by_pregnancy_months", "flu_by_pregnancy_months_cat2", 
         "total_covid_hosp", "covid_hosp_by_pregn_month",
         "covid_hosp_by_pregn_month_cat2")


# Removing useless variables
bevn_eco <- bevn_eco %>%
  select(-one_of("monthly_hospitalizations_month1",
                 "monthly_hospitalizations_month2", "monthly_hospitalizations_month3",
                 "monthly_hospitalizations_month4",  "monthly_hospitalizations_month5",  "monthly_hospitalizations_month6", 
                 "monthly_hospitalizations_month7",  "monthly_hospitalizations_month8",  "monthly_hospitalizations_month9", 
                 "monthly_hospitalizations_birthmonth"))



# 2. TRIMESTER EXPOSURE
######################################### Heatwaves ###############################################################
#Heatwaves: Canicule européenne de 2015 29/06 until 08/07
#2018 : aout
#2019 Canicule européenne de 2019 23/06 – 01/08 (not included here) 

# Exposure during one of the pregnancy trimesters
  ## Exposure during first trimester (heatwave 2015 or 2018, independently)
  bevn_eco <- bevn_eco %>%
  dplyr::mutate(HW_first_trimester=ifelse((month_1=="2015-07-01" | month_1=="2018-08-01"| 
                                          month_2=="2015-07-01" |month_2=="2018-08-01"| 
                                          month_3=="2015-07-01" |month_3=="2018-08-01") 
                                             , '1',
                                             0),
                HW_first_trimester=as.factor(HW_first_trimester),
                HW_first_trimester= replace(HW_first_trimester,
                                           (is.na(HW_first_trimester)), 0))

  ## Exposure during second trimester (heatwave 2015 or 2018, independently)
  # bevn_eco <- bevn_eco %>%
  # dplyr::mutate(HW_second_trimester=ifelse((month_4=="2015-07-01" | month_4=="2018-08-01"| 
  #                                           month_5=="2015-07-01" |month_5=="2018-08-01"| 
  #                                           month_6=="2015-07-01" |month_6=="2018-08-01") 
  #                                          , '1',
  #                                          0),
  #               HW_second_trimester=as.factor(HW_second_trimester),
  #               HW_second_trimester= replace(HW_second_trimester,
  #                                             (is.na(HW_second_trimester)), 0))
  ## Exposure during LAST trimester (heatwave 2015 or 2018, independently) - up to delivery
bevn_eco <- bevn_eco %>%
  dplyr::mutate(HW_last_trimester=ifelse(
                (GA_month==4 & (month_3=="2015-07-01" | month_3=="2018-08-01"| 
                                month_4=="2015-07-01" |month_4=="2018-08-01")) |
                (GA_month==5 & (month_4=="2015-07-01" | month_4=="2018-08-01"| 
                               month_5=="2015-07-01" |month_5=="2018-08-01")) |
                (GA_month==6 & (month_5=="2015-07-01" | month_5=="2018-08-01"| 
                              month_6=="2015-07-01" |month_6=="2018-08-01")) |
                (GA_month==7 & (month_6=="2015-07-01" | month_6=="2018-08-01"| 
                                month_7=="2015-07-01" |month_7=="2018-08-01")) |
                (GA_month==8 & (month_7=="2015-07-01" | month_7=="2018-08-01"| 
                                month_8=="2015-07-01" |month_8=="2018-08-01")) |
                (GA_month==9 & (month_8=="2015-07-01" | month_8=="2018-08-01"| 
                                month_9=="2015-07-01" |month_9=="2018-08-01")) |
                (GA_month==10 & (month_9=="2015-07-01" | month_9=="2018-08-01"| 
                                 month_10=="2015-07-01" |month_10=="2018-08-01")) |
                (GA_month==11 & (month_10=="2015-07-01" | month_10=="2018-08-01"| 
                                month_11=="2015-07-01" |month_11=="2018-08-01")) |
                 birth_Y_M_1stday=="2015-07-01" |birth_Y_M_1stday=="2018-08-01"
               , '1', 0),
                HW_last_trimester=as.factor(HW_last_trimester),
                HW_last_trimester= replace(HW_last_trimester,
                                           (is.na(HW_last_trimester)), 0))

# bevn_eco <- bevn_eco %>%
#   dplyr::mutate(HW_third_trimester=ifelse((month_7=="2015-07-01" | month_7=="2018-08-01"| 
#                                             month_8=="2015-07-01" |month_8=="2018-08-01"| 
#                                             month_9=="2015-07-01" |month_9=="2018-08-01" |
#                                             month_10=="2015-07-01" |month_10=="2018-08-01"|
#                                             month_11=="2015-07-01" |month_11=="2018-08-01" |
#                                             birth_Y_M_1stday=="2015-07-01" |birth_Y_M_1stday =="2018-08-01") 
#                                              , '1',
#                                              0),
#                 HW_third_trimester=as.factor(HW_third_trimester),
#                 HW_third_trimester= replace(HW_third_trimester,
#                                             (is.na(HW_third_trimester)), 0))
  
 
 
########################################## Economic crises ###############################################################
# Great recession: Q4 2008 and Q1 2009
# bevn_eco <- bevn_eco %>%
#   dplyr::mutate(GR=ifelse((birthyear==2008 & (birthmonth==10 | birthmonth==11 | birthmonth==12))
#                           | (birthyear==2009 & (birthmonth==1 | birthmonth==2 | birthmonth==3))
#                           , '1',
#                           0),
#                 GR=as.factor(GR))
# table(bevn_eco$birthmonth, bevn_eco$birthyear, bevn_eco$GR)

# Exposure during one of the pregnancy trimesters
  ## Exposure during first trimester
  # bevn_eco <- bevn_eco %>%
  #   mutate(GR_first_trimester = ifelse(month_1 >= as.Date("2008-10-01") & month_1 <= as.Date("2009-03-01") |
  #                                      (month_2 >= as.Date("2008-10-01") & month_2 <= as.Date("2009-03-01")) |
  #                                      (month_3 >= as.Date("2008-10-01") & month_3 <= as.Date("2009-03-01")), 1, 0),
  #          GR_first_trimester = as.factor(GR_first_trimester),
  #          GR_first_trimester= replace(GR_first_trimester,
  #                                       (is.na(GR_first_trimester)), 0))

bevn_eco <- bevn_eco %>%
  mutate(GR_first_trimester_continuous = case_when((month_1 >= as.Date("2008-10-01") & month_1 <= as.Date("2009-03-01") &
                                       (month_2 >= as.Date("2008-10-01") & month_2 <= as.Date("2009-03-01")) &
                                       (month_3 >= as.Date("2008-10-01") & month_3 <= as.Date("2009-03-01")))~ 3,
                                       
                                       ((month_1 >= as.Date("2008-10-01") & month_1 <= as.Date("2009-03-01") &
                                        !(month_2 >= as.Date("2008-10-01") & month_2 <= as.Date("2009-03-01")))|
                                          
                                          !(month_1 >= as.Date("2008-10-01") & month_1 <= as.Date("2009-03-01")) &
                                          !(month_2 >= as.Date("2008-10-01") & month_2 <= as.Date("2009-03-01")) &
                                          (month_3 >= as.Date("2008-10-01") & month_3 <= as.Date("2009-03-01")))~ 1,
                                       
                                       ((month_1 >= as.Date("2008-10-01") & month_1 <= as.Date("2009-03-01") &
                                        (month_2 >= as.Date("2008-10-01") & month_2 <= as.Date("2009-03-01")) &
                                        !(month_3 >= as.Date("2008-10-01") & month_3 <= as.Date("2009-03-01")))|
                                          
                                          !(month_1 >= as.Date("2008-10-01") & month_1 <= as.Date("2009-03-01")) &
                                          (month_2 >= as.Date("2008-10-01") & month_2 <= as.Date("2009-03-01")) &
                                          (month_3 >= as.Date("2008-10-01") & month_3 <= as.Date("2009-03-01")))~ 2,
                                       
                                       !(month_1 >= as.Date("2008-10-01") & month_1 <= as.Date("2009-03-01")) &
                                        !(month_2 >= as.Date("2008-10-01") & month_2 <= as.Date("2009-03-01")) &
                                        !(month_3 >= as.Date("2008-10-01") & month_3 <= as.Date("2009-03-01")) ~
                                       0),
         GR_first_trimester_continuous = as.numeric(GR_first_trimester_continuous))
table(bevn_eco$GR_first_trimester_continuous, useNA = "always")
  ## Exposure during second trimester
  # bevn_eco <- bevn_eco %>%
  #   mutate(GR_second_trimester = ifelse(month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01") |
  #                                        (month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01")) |
  #                                        (month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01")), 1, 0),
  #          GR_second_trimester = as.factor(GR_second_trimester),
  #          GR_second_trimester= replace(GR_second_trimester,
  #                                      (is.na(GR_second_trimester)), 0))

## Exposure during last trimester - up to delivery
# bevn_eco <- bevn_eco %>%
#   mutate(GR_last_trimester = ifelse(
#     (GA_month==4 & (month_3 >= as.Date("2008-10-01") & month_3 <= as.Date("2009-03-01") |
#                     (month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01")))) |
#     (GA_month==5 & (month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01") |
#                         (month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01")))) |
#     (GA_month==6 & (month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01") |
#                         (month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01")))) |
#       (GA_month==7 & (month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01") |
#                         (month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01")))) |
#       (GA_month==8 & (month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01") |
#                         (month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01")))) |
#       (GA_month==9 & (month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01") |
#                         (month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01")))) |
#       (GA_month==10 & (month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01") |
#                         (month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01")))) |
#       (GA_month==11 & (month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01") |
#                          (month_11 >= as.Date("2008-10-01") & month_11 <= as.Date("2009-03-01")))) |
#       (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01") |
#          (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01")))
#     , '1', 0),
#     GR_last_trimester = as.factor(GR_last_trimester),
#     GR_last_trimester= replace(GR_last_trimester,
#                                 (is.na(GR_last_trimester)), 0))

bevn_eco <- bevn_eco %>%
  mutate(
    GR_last_trimester_continuous = ifelse(GA_month == 4,
      case_when(
        (month_3 >= as.Date("2008-10-01") & month_3 <= as.Date("2009-03-01") &
           (month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01")) &
           (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 3,
        
        ((month_3 >= as.Date("2008-10-01") & month_3 <= as.Date("2009-03-01") &
            !(month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01"))) |
           !(month_3 >= as.Date("2008-10-01") & month_3 <= as.Date("2009-03-01")) &
           !(month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01")) &
           (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 1,
        
        ((month_3 >= as.Date("2008-10-01") & month_3 <= as.Date("2009-03-01") &
            (month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01")) &
            !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) |
           !(month_3 >= as.Date("2008-10-01") & month_3 <= as.Date("2009-03-01")) &
           (month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01")) &
           (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 2,

        !(month_3 >= as.Date("2008-10-01") & month_3 <= as.Date("2009-03-01")) &
          !(month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01")) &
          !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01")) ~
          0
        ),
      ifelse(GA_month == 5,
        case_when(
          (month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01") &
             (month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01")) &
             (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 3,
          
          ((month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01") &
              !(month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01"))) |
             !(month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01")) &
             !(month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01")) &
             (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 1,
          
          ((month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01") &
              (month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01")) &
              !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) |
             !(month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01")) &
             (month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01")) &
             (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 2,
          
          !(month_4 >= as.Date("2008-10-01") & month_4 <= as.Date("2009-03-01")) &
            !(month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01")) &
            !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01")) ~
            0
          ), 
        ifelse(GA_month == 6,
          case_when(
            (month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01") &
               (month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01")) &
               (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 3,
            
            ((month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01") &
                !(month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01"))) |
               !(month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01")) &
               !(month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01")) &
               (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 1,
            
            ((month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01") &
                (month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01")) &
                !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) |
               !(month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01")) &
               (month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01")) &
               (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 2,
            
            !(month_5 >= as.Date("2008-10-01") & month_5 <= as.Date("2009-03-01")) &
              !(month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01")) &
              !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01")) ~
              0
          ), 
      ifelse(GA_month == 7,
            case_when(
              (month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01") &
                 (month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01")) &
                 (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 3,
              
              ((month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01") &
                  !(month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01"))) |
                 !(month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01")) &
                 !(month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01")) &
                 (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 1,
              
              ((month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01") &
                  (month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01")) &
                  !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) |
                 !(month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01")) &
                 (month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01")) &
                 (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 2,
              
              !(month_6 >= as.Date("2008-10-01") & month_6 <= as.Date("2009-03-01")) &
                !(month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01")) &
                !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01")) ~
                0
            ),           
          ifelse(GA_month == 8,
              case_when(
                (month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01") &
                   (month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01")) &
                   (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 3,
                
                ((month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01") &
                    !(month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01"))) |
                   !(month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01")) &
                   !(month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01")) &
                   (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 1,
                
                ((month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01") &
                    (month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01")) &
                    !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) |
                   !(month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01")) &
                   (month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01")) &
                   (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 2,
                
                !(month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01")) &
                  !(month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01")) &
                  !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01")) ~
                  0
              ),      
        ifelse(GA_month == 9,
                case_when(
                  (month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01") &
                     (month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01")) &
                     (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 3,
                  
                  ((month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01") &
                      !(month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01"))) |
                     !(month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01")) &
                     !(month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01")) &
                     (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 1,
                  
                  ((month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01") &
                      (month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01")) &
                      !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) |
                     !(month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01")) &
                     (month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01")) &
                     (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 2,
                  
                  !(month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01")) &
                    !(month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01")) &
                    !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01")) ~
                    0
                ),    
     ifelse(GA_month == 10,
                  case_when(
                    (month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01") &
                       (month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01")) &
                       (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 3,
                    
                    ((month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01") &
                        !(month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01"))) |
                       !(month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01")) &
                       !(month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01")) &
                       (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 1,
                    
                    ((month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01") &
                        (month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01")) &
                        !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) |
                       !(month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01")) &
                       (month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01")) &
                       (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 2,
                    
                    !(month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01")) &
                      !(month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01")) &
                      !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01")) ~
                      0
                  ),       
    ifelse(GA_month == 11,
            case_when(
                   (month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01") &
                         (month_11 >= as.Date("2008-10-01") & month_11 <= as.Date("2009-03-01")) &
                         (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 3,
                      
                    ((month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01") &
                          !(month_11 >= as.Date("2008-10-01") & month_11 <= as.Date("2009-03-01"))) |
                         !(month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01")) &
                         !(month_11 >= as.Date("2008-10-01") & month_11 <= as.Date("2009-03-01")) &
                         (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 1,
                      
                    ((month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01") &
                          (month_11 >= as.Date("2008-10-01") & month_11 <= as.Date("2009-03-01")) &
                          !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) |
                         !(month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01")) &
                         (month_11 >= as.Date("2008-10-01") & month_11 <= as.Date("2009-03-01")) &
                         (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01"))) ~ 2,
                      
                    !(month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01")) &
                        !(month_11 >= as.Date("2008-10-01") & month_11 <= as.Date("2009-03-01")) &
                        !(birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01")) ~
                        0
                    ),              
                    
                
        NA
      )))))))),
    GR_last_trimester_continuous = as.numeric(GR_last_trimester_continuous),
  )

table(bevn_eco$GR_first_trimester_continuous,useNA = "always")
table(bevn_eco$GR_last_trimester_continuous,useNA = "always")

## Exposure during third trimester - up to delivery
  # bevn_eco <- bevn_eco %>%
  #   mutate(GR_third_trimester = ifelse(month_7 >= as.Date("2008-10-01") & month_7 <= as.Date("2009-03-01") |
  #                                         (month_8 >= as.Date("2008-10-01") & month_8 <= as.Date("2009-03-01")) |
  #                                         (month_9 >= as.Date("2008-10-01") & month_9 <= as.Date("2009-03-01")) |
  #                                         (month_10 >= as.Date("2008-10-01") & month_10 <= as.Date("2009-03-01")) |
  #                                         (month_11 >= as.Date("2008-10-01") & month_11 <= as.Date("2009-03-01")) |
  #                                         (birth_Y_M_1stday >= as.Date("2008-10-01") & birth_Y_M_1stday <= as.Date("2009-03-01")), 1, 0),
  #          GR_third_trimester = as.factor(GR_third_trimester),
  #          GR_third_trimester= replace(GR_third_trimester,
  #                                       (is.na(GR_third_trimester)), 0))
  
###################### Flu ####################################################################################### 
  # Exposure during one of the pregnancy trimesters
  ## Exposure during first trimester
  bevn_eco <- bevn_eco %>%
    mutate(flu_first_trimester = ifelse(month_1 >= as.Date("2009-10-01") & month_1 <= as.Date("2010-01-01") |
                                  (month_2 >= as.Date("2009-10-01") & month_2 <= as.Date("2010-01-01")) |
                                  (month_3 >= as.Date("2009-10-01") & month_3 <= as.Date("2010-01-01")), 1, 0),
            flu_first_trimester = as.factor(flu_first_trimester),
    flu_first_trimester= replace(flu_first_trimester,
                                              (is.na(flu_first_trimester)), 0))
  
  bevn_eco <- bevn_eco %>%
    mutate(flu_second_trimester = ifelse(month_4 >= as.Date("2009-10-01") & month_4 <= as.Date("2010-01-01")|
                                        (month_5 >= as.Date("2009-10-01") & month_5 <= as.Date("2010-01-01")) |
                                        (month_6 >= as.Date("2009-10-01") & month_6 <= as.Date("2010-01-01")), 1, 0),
           flu_second_trimester = as.factor(flu_second_trimester),
           flu_second_trimester= replace(flu_second_trimester,
                                        (is.na(flu_second_trimester)), 0))
  
  ## Exposure during third trimester - up to delivery incl.
  bevn_eco <- bevn_eco %>%
    mutate(flu_third_trimester = ifelse(month_7 >= as.Date("2009-10-01") & month_7 <= as.Date("2010-01-01") |
                                        (month_8 >= as.Date("2009-10-01") & month_8 <= as.Date("2010-01-01")) |
                                        (month_9 >= as.Date("2009-10-01") & month_9 <= as.Date("2010-01-01")) |
                                        (month_10 >= as.Date("2009-10-01") & month_10 <= as.Date("2010-01-01")) |
                                        (month_11 >= as.Date("2009-10-01") & month_11 <= as.Date("2010-01-01")) |
                                        (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01")), 1, 0),
           flu_third_trimester = as.factor(flu_third_trimester),
           flu_third_trimester= replace(flu_third_trimester,
                                         (is.na(flu_third_trimester)), 0))

  
  bevn_eco <- bevn_eco %>%
    mutate(flu_first_trimester_continuous = case_when((month_1 >= as.Date("2009-10-01") & month_1 <= as.Date("2010-01-01") &
                                                        (month_2 >= as.Date("2009-10-01") & month_2 <= as.Date("2010-01-01")) &
                                                        (month_3 >= as.Date("2009-10-01") & month_3 <= as.Date("2010-01-01")))~ 3,
                                                     
                                                     ((month_1 >= as.Date("2009-10-01") & month_1 <= as.Date("2010-01-01") &
                                                         !(month_2 >= as.Date("2009-10-01") & month_2 <= as.Date("2010-01-01")))|
                                                        
                                                        !(month_1 >= as.Date("2009-10-01") & month_1 <= as.Date("2010-01-01")) &
                                                        !(month_2 >= as.Date("2009-10-01") & month_2 <= as.Date("2010-01-01")) &
                                                        (month_3 >= as.Date("2009-10-01") & month_3 <= as.Date("2010-01-01")))~ 1,
                                                     
                                                     ((month_1 >= as.Date("2009-10-01") & month_1 <= as.Date("2010-01-01") &
                                                         (month_2 >= as.Date("2009-10-01") & month_2 <= as.Date("2010-01-01")) &
                                                         !(month_3 >= as.Date("2009-10-01") & month_3 <= as.Date("2010-01-01")))|
                                                        
                                                        !(month_1 >= as.Date("2009-10-01") & month_1 <= as.Date("2010-01-01")) &
                                                        (month_2 >= as.Date("2009-10-01") & month_2 <= as.Date("2010-01-01")) &
                                                        (month_3 >= as.Date("2009-10-01") & month_3 <= as.Date("2010-01-01")))~ 2,
                                                     
                                                     !(month_1 >= as.Date("2009-10-01") & month_1 <= as.Date("2010-01-01")) &
                                                       !(month_2 >= as.Date("2009-10-01") & month_2 <= as.Date("2010-01-01")) &
                                                       !(month_3 >= as.Date("2009-10-01") & month_3 <= as.Date("2010-01-01")) ~
                                                       0),
           flu_first_trimester_continuous = as.numeric(flu_first_trimester_continuous))
  table(bevn_eco$flu_first_trimester_continuous, useNA = "always")
  
  
  # Flu last trimester continuous
  bevn_eco <- bevn_eco %>%
    mutate(
      flu_last_trimester_continuous = ifelse(GA_month == 4,
                                            case_when(
                                       (month_3 >= as.Date("2009-10-01") & month_3 <= as.Date("2010-01-01") &
                                        (month_4 >= as.Date("2009-10-01") & month_4 <= as.Date("2010-01-01")) &
                                        (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 3,
                                              
                                        ((month_3 >= as.Date("2009-10-01") & month_3 <= as.Date("2010-01-01") &
                                        !(month_4 >= as.Date("2009-10-01") & month_4 <= as.Date("2010-01-01"))) |
                                        !(month_3 >= as.Date("2009-10-01") & month_3 <= as.Date("2010-01-01")) &
                                        !(month_4 >= as.Date("2009-10-01") & month_4 <= as.Date("2010-01-01")) &
                                        (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 1,
                                              
                                        ((month_3 >= as.Date("2009-10-01") & month_3 <= as.Date("2010-01-01") &
                                        (month_4 >= as.Date("2009-10-01") & month_4 <= as.Date("2010-01-01")) &
                                        !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) |
                                        !(month_3 >= as.Date("2009-10-01") & month_3 <= as.Date("2010-01-01")) &
                                       (month_4 >= as.Date("2009-10-01") & month_4 <= as.Date("2010-01-01")) &
                                       (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 2,
                                              
                                        !(month_3 >= as.Date("2009-10-01") & month_3 <= as.Date("2010-01-01")) &
                                       !(month_4 >= as.Date("2009-10-01") & month_4 <= as.Date("2010-01-01")) &
                                        !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01")) ~
                                                0
                                        ),
                                    ifelse(GA_month == 5,
                                           case_when(
                                           (month_4 >= as.Date("2009-10-01") & month_4 <= as.Date("2010-01-01") &
                                           (month_5 >= as.Date("2009-10-01") & month_5 <= as.Date("2010-01-01")) &
                                           (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 3,
                                                     
                                           ((month_4 >= as.Date("2009-10-01") & month_4 <= as.Date("2010-01-01") &
                                           !(month_5 >= as.Date("2009-10-01") & month_5 <= as.Date("2010-01-01"))) |
                                           !(month_4 >= as.Date("2009-10-01") & month_4 <= as.Date("2010-01-01")) &
                                           !(month_5 >= as.Date("2009-10-01") & month_5 <= as.Date("2010-01-01")) &
                                           (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 1,
                                                     
                                           ((month_4 >= as.Date("2009-10-01") & month_4 <= as.Date("2010-01-01") &
                                           (month_5 >= as.Date("2009-10-01") & month_5 <= as.Date("2010-01-01")) &
                                           !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) |
                                           !(month_4 >= as.Date("2009-10-01") & month_4 <= as.Date("2010-01-01")) &
                                           (month_5 >= as.Date("2009-10-01") & month_5 <= as.Date("2010-01-01")) &
                                           (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 2,
                                                     
                                            !(month_4 >= as.Date("2009-10-01") & month_4 <= as.Date("2010-01-01")) &
                                            !(month_5 >= as.Date("2009-10-01") & month_5 <= as.Date("2010-01-01")) &
                                            !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01")) ~
                                             0
                                            ), 
                                      ifelse(GA_month == 6,
                                             case_when(
                                             (month_5 >= as.Date("2009-10-01") & month_5 <= as.Date("2010-01-01") &
                                             (month_6 >= as.Date("2009-10-01") & month_6 <= as.Date("2010-01-01")) &
                                             (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 3,
                                                            
                                             ((month_5 >= as.Date("2009-10-01") & month_5 <= as.Date("2010-01-01") &
                                              !(month_6 >= as.Date("2009-10-01") & month_6 <= as.Date("2010-01-01"))) |
                                              !(month_5 >= as.Date("2009-10-01") & month_5 <= as.Date("2010-01-01")) &
                                              !(month_6 >= as.Date("2009-10-01") & month_6 <= as.Date("2010-01-01")) &
                                              (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 1,
                                                            
                                              ((month_5 >= as.Date("2009-10-01") & month_5 <= as.Date("2010-01-01") &
                                               (month_6 >= as.Date("2009-10-01") & month_6 <= as.Date("2010-01-01")) &
                                               !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) |
                                               !(month_5 >= as.Date("2009-10-01") & month_5 <= as.Date("2010-01-01")) &
                                               (month_6 >= as.Date("2009-10-01") & month_6 <= as.Date("2010-01-01")) &
                                               (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 2,
                                                            
                                               !(month_5 >= as.Date("2009-10-01") & month_5 <= as.Date("2010-01-01")) &
                                               !(month_6 >= as.Date("2009-10-01") & month_6 <= as.Date("2010-01-01")) &
                                               !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01")) ~
                                               0
                                                ), 
                                      ifelse(GA_month == 7,
                                             case_when(
                                             (month_6 >= as.Date("2009-10-01") & month_6 <= as.Date("2010-01-01") &
                                             (month_7 >= as.Date("2009-10-01") & month_7 <= as.Date("2010-01-01")) &
                                             (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 3,
                                                                   
                                             ((month_6 >= as.Date("2009-10-01") & month_6 <= as.Date("2010-01-01") &
                                             !(month_7 >= as.Date("2009-10-01") & month_7 <= as.Date("2010-01-01"))) |
                                             !(month_6 >= as.Date("2009-10-01") & month_6 <= as.Date("2010-01-01")) &
                                             !(month_7 >= as.Date("2009-10-01") & month_7 <= as.Date("2010-01-01")) &
                                             (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 1,
                                                                   
                                             ((month_6 >= as.Date("2009-10-01") & month_6 <= as.Date("2010-01-01") &
                                             (month_7 >= as.Date("2009-10-01") & month_7 <= as.Date("2010-01-01")) &
                                             !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) |
                                             !(month_6 >= as.Date("2009-10-01") & month_6 <= as.Date("2010-01-01")) &
                                             (month_7 >= as.Date("2009-10-01") & month_7 <= as.Date("2010-01-01")) &
                                             (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 2,
                                                                   
                                             !(month_6 >= as.Date("2009-10-01") & month_6 <= as.Date("2010-01-01")) &
                                            !(month_7 >= as.Date("2009-10-01") & month_7 <= as.Date("2010-01-01")) &
                                             !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01")) ~
                                             0
                                             ),           
                                    ifelse(GA_month == 8,
                                           case_when(
                                           (month_7 >= as.Date("2009-10-01") & month_7 <= as.Date("2010-01-01") &
                                           (month_8 >= as.Date("2009-10-01") & month_8 <= as.Date("2010-01-01")) &
                                           (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 3,
                                                                          
                                           ((month_7 >= as.Date("2009-10-01") & month_7 <= as.Date("2010-01-01") &
                                          !(month_8 >= as.Date("2009-10-01") & month_8 <= as.Date("2010-01-01"))) |
                                            !(month_7 >= as.Date("2009-10-01") & month_7 <= as.Date("2010-01-01")) &
                                           !(month_8 >= as.Date("2009-10-01") & month_8 <= as.Date("2010-01-01")) &
                                            (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 1,
                                                                          
                                           ((month_7 >= as.Date("2009-10-01") & month_7 <= as.Date("2010-01-01") &
                                            (month_8 >= as.Date("2009-10-01") & month_8 <= as.Date("2010-01-01")) &
                                            !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) |
                                             !(month_7 >= as.Date("2009-10-01") & month_7 <= as.Date("2010-01-01")) &
                                              (month_8 >= as.Date("2009-10-01") & month_8 <= as.Date("2010-01-01")) &
                                             (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 2,
                                                                          
                                          !(month_7 >= as.Date("2009-10-01") & month_7 <= as.Date("2010-01-01")) &
                                           !(month_8 >= as.Date("2009-10-01") & month_8 <= as.Date("2010-01-01")) &
                                           !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01")) ~
                                           0
                                           ),      
                                   ifelse(GA_month == 9,
                                          case_when(
                                          (month_8 >= as.Date("2009-10-01") & month_8 <= as.Date("2010-01-01") &
                                          (month_9 >= as.Date("2009-10-01") & month_9 <= as.Date("2010-01-01")) &
                                          (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 3,
                                                                                 
                                          ((month_8 >= as.Date("2009-10-01") & month_8 <= as.Date("2010-01-01") &
                                          !(month_9 >= as.Date("2009-10-01") & month_9 <= as.Date("2010-01-01"))) |
                                          !(month_8 >= as.Date("2009-10-01") & month_8 <= as.Date("2010-01-01")) &
                                          !(month_9 >= as.Date("2009-10-01") & month_9 <= as.Date("2010-01-01")) &
                                          (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 1,
                                                                                 
                                           ((month_8 >= as.Date("2009-10-01") & month_8 <= as.Date("2010-01-01") &
                                          (month_9 >= as.Date("2009-10-01") & month_9 <= as.Date("2010-01-01")) &
                                          !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) |
                                          !(month_8 >= as.Date("2009-10-01") & month_8 <= as.Date("2010-01-01")) &
                                           (month_9 >= as.Date("2009-10-01") & month_9 <= as.Date("2010-01-01")) &
                                           (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 2,
                                                                                 
                                           !(month_8 >= as.Date("2009-10-01") & month_8 <= as.Date("2010-01-01")) &
                                           !(month_9 >= as.Date("2009-10-01") & month_9 <= as.Date("2010-01-01")) &
                                          !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01")) ~
                                           0
                                          ),    
                                    ifelse(GA_month == 10,
                                           case_when(
                                            (month_9 >= as.Date("2009-10-01") & month_9 <= as.Date("2010-01-01") &
                                           (month_10 >= as.Date("2009-10-01") & month_10 <= as.Date("2010-01-01")) &
                                           (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 3,
                                                                                        
                                           ((month_9 >= as.Date("2009-10-01") & month_9 <= as.Date("2010-01-01") &
                                           !(month_10 >= as.Date("2009-10-01") & month_10 <= as.Date("2010-01-01"))) |
                                           !(month_9 >= as.Date("2009-10-01") & month_9 <= as.Date("2010-01-01")) &
                                           !(month_10 >= as.Date("2009-10-01") & month_10 <= as.Date("2010-01-01")) &
                                           (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 1,
                                                                                        
                                           ((month_9 >= as.Date("2009-10-01") & month_9 <= as.Date("2010-01-01") &
                                           (month_10 >= as.Date("2009-10-01") & month_10 <= as.Date("2010-01-01")) &
                                           !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) |
                                           !(month_9 >= as.Date("2009-10-01") & month_9 <= as.Date("2010-01-01")) &
                                           (month_10 >= as.Date("2009-10-01") & month_10 <= as.Date("2010-01-01")) &
                                           (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 2,
                                                                                        
                                           !(month_9 >= as.Date("2009-10-01") & month_9 <= as.Date("2010-01-01")) &
                                          !(month_10 >= as.Date("2009-10-01") & month_10 <= as.Date("2010-01-01")) &
                                            !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01")) ~
                                           0
                                           ),       
                                    ifelse(GA_month == 11,
                                           case_when(
                                           (month_10 >= as.Date("2009-10-01") & month_10 <= as.Date("2010-01-01") &
                                           (month_11 >= as.Date("2009-10-01") & month_11 <= as.Date("2010-01-01")) &
                                           (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 3,
                                                                                               
                                            ((month_10 >= as.Date("2009-10-01") & month_10 <= as.Date("2010-01-01") &
                                            !(month_11 >= as.Date("2009-10-01") & month_11 <= as.Date("2010-01-01"))) |
                                           !(month_10 >= as.Date("2009-10-01") & month_10 <= as.Date("2010-01-01")) &
                                            !(month_11 >= as.Date("2009-10-01") & month_11 <= as.Date("2010-01-01")) &
                                           (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 1,
                                                                                               
                                            ((month_10 >= as.Date("2009-10-01") & month_10 <= as.Date("2010-01-01") &
                                             (month_11 >= as.Date("2009-10-01") & month_11 <= as.Date("2010-01-01")) &
                                             !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) |
                                             !(month_10 >= as.Date("2009-10-01") & month_10 <= as.Date("2010-01-01")) &
                                             (month_11 >= as.Date("2009-10-01") & month_11 <= as.Date("2010-01-01")) &
                                             (birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01"))) ~ 2,
                                                                                               
                                             !(month_10 >= as.Date("2009-10-01") & month_10 <= as.Date("2010-01-01")) &
                                              !(month_11 >= as.Date("2009-10-01") & month_11 <= as.Date("2010-01-01")) &
                                            !(birth_Y_M_1stday >= as.Date("2009-10-01") & birth_Y_M_1stday <= as.Date("2010-01-01")) ~
                                              0
                                             ),              
                                                                                             
                                            NA
                                                                                      )))))))),
      flu_last_trimester_continuous = as.numeric(flu_last_trimester_continuous)
    )
  
  table(bevn_eco$flu_last_trimester_continuous,useNA = "always")
  
########################################## COVID-19 ###############################################################
# Delivery date during COVID19
  ## Delivery date during either wave (detail: wave 1: 03 and 04/2020, wave 2: 10/2020 to 02/2021, wave 3: 11 and 12/2021)
# bevn_eco <- bevn_eco %>%
#   dplyr::mutate(COVID=ifelse(((birthyear==2020 & (birthmonth==3 | birthmonth==4))| # first wave
#                                 (birthyear==2020 & (birthmonth==10 | birthmonth==11 | birthmonth ==12)) # second wave
#                                  | (birthyear==2021 & (birthmonth==01 |birthmonth==02|# second wave
#                                  birthmonth==11 | birthmonth==12))
#                                  ) # third wave
#                           , '1',
#                           0),
#                 COVID=as.factor(COVID))
# table(bevn_eco$birthmonth, bevn_eco$birthyear, bevn_eco$COVID)
# #   
# Exposure during one of the pregnancy trimesters
  ## Exposure during first trimester (independently of each of the three waves)
  # bevn_eco <- bevn_eco %>%
  # dplyr::mutate(COVID_first_trimester=ifelse((month_1=="2020-03-01" | month_1=="2020-04-01" | # 1st wave
  #                                             (month_1 >= as.Date("2020-10-01") & month_1 <= as.Date("2021-02-01")) | # 2nd wave
  #                                             month_1=="2021-11-01" | month_1=="2021-12-01" | # 3rd wave
  #                                             month_2=="2020-03-01" | month_2=="2020-04-01" | # 1st wave
  #                                             (month_2 >= as.Date("2020-10-01") & month_2 <= as.Date("2021-02-01")) | # 2nd wave
  #                                             month_2=="2021-11-01" | month_2=="2021-12-01" | # 3rd wave
  #                                             month_3=="2020-03-01" | month_3=="2020-04-01" | # 1st wave
  #                                             (month_3 >= as.Date("2020-10-01") & month_3 <= as.Date("2021-02-01")) | # 2nd wave
  #                                             month_3=="2021-11-01" | month_3=="2021-12-01" # 3rd wave
  #                                             )
  #                                        , '1',
  #                                        0),
  #               COVID_first_trimester=as.factor(COVID_first_trimester),
  #               COVID_first_trimester= replace(COVID_first_trimester,
  #                                                 (is.na(COVID_first_trimester)), 0))
  # 
  # # ## Exposure during second trimester (independently of each of the three waves)
  # bevn_eco <- bevn_eco %>%
  # dplyr::mutate(COVID_second_trimester=ifelse((month_4=="2020-03-01" | month_4=="2020-04-01" | # 1st wave
  #                                             (month_4 >= as.Date("2020-10-01") & month_4 <= as.Date("2021-02-01")) | # 2nd wave
  #                                               month_4=="2021-11-01" | month_4=="2021-12-01" | # 3rd wave
  #                                               month_5=="2020-03-01" | month_5=="2020-04-01" | # 1st wave
  #                                               (month_5 >= as.Date("2020-10-01") & month_5 <= as.Date("2021-02-01")) | # 2nd wave
  #                                               month_5=="2021-11-01" | month_5=="2021-12-01" | # 3rd wave
  #                                               month_6=="2020-03-01" | month_6=="2020-04-01" | # 1st wave
  #                                               (month_6 >= as.Date("2020-10-01") & month_6 <= as.Date("2021-02-01")) | # 2nd wave
  #                                               month_6=="2021-11-01" | month_6=="2021-12-01" # 3rd wave
  #                                               )
  #                                         , '1',
  #                                        0),
  #               COVID_second_trimester=as.factor(COVID_second_trimester),
  #               COVID_second_trimester= replace(COVID_second_trimester,
  #                                                 (is.na(COVID_second_trimester)), 0))
  # 
  # # ## Exposure during third trimester (independently of each of the three waves) - up to delivery month 7 to 11
  # bevn_eco <- bevn_eco %>%
  # dplyr::mutate(COVID_third_trimester=ifelse((month_7=="2020-03-01" | month_7=="2020-04-01" | # 1st wave
  #                                             (month_7 >= as.Date("2020-10-01") & month_7 <= as.Date("2021-02-01")) | # 2nd wave
  #                                               month_7=="2021-11-01" | month_7=="2021-12-01" | # 3rd wave
  #                                               month_8=="2020-03-01" | month_8=="2020-04-01" | # 1st wave
  #                                               (month_8 >= as.Date("2020-10-01") & month_8 <= as.Date("2021-02-01")) | # 2nd wave
  #                                               month_8=="2021-11-01" | month_8=="2021-12-01" | # 3rd wave
  #                                               month_9=="2020-03-01" | month_9=="2020-04-01" | # 1st wave
  #                                               (month_9 >= as.Date("2020-10-01") & month_9 <= as.Date("2021-02-01")) | # 2nd wave
  #                                               month_9=="2021-11-01" | month_9=="2021-12-01" | # 3rd wave
  #                                               month_10=="2020-03-01" | month_10=="2020-04-01" | # 1st wave
  #                                               (month_10 >= as.Date("2020-10-01") & month_10 <= as.Date("2021-02-01")) | # 2nd wave
  #                                               month_10=="2021-11-01" | month_10=="2021-12-01" | # 3rd wave
  #                                               month_11=="2020-03-01" | month_11=="2020-04-01" | # 1st wave
  #                                               (month_11 >= as.Date("2020-10-01") & month_11 <= as.Date("2021-02-01")) | # 2nd wave
  #                                               month_11=="2021-11-01" | month_11=="2021-12-01" | #3rd wave
  #                                               birth_Y_M_1stday=="2020-03-01" | birth_Y_M_1stday=="2020-04-01" | # 1st wave
  #                                               (birth_Y_M_1stday >= as.Date("2020-10-01") & birth_Y_M_1stday <= as.Date("2021-02-01")) | # 2nd wave
  #                                               birth_Y_M_1stday=="2021-11-01" | birth_Y_M_1stday=="2021-12-01"
  #                                               )
  #                                             , '1',
  #                                             0),
  #               COVID_third_trimester=as.factor(COVID_third_trimester),
  #               COVID_third_trimester= replace(COVID_third_trimester,
  #                                             (is.na(COVID_third_trimester)), 0))


  
# 2023.09.13
  # exposure during 3rd trimester vs not exposed (the "not exposed" DO NOT include the 
  #exposed in 1st/2nd trimester --> these are put to NA)
# bevn_eco <- bevn_eco %>%
#    mutate(HW_3rd_trim_vs_not_exposed=ifelse(HW_first_trimester==0 &
#                                             HW_second_trimester==0 &
#                                             HW_third_trimester==1, '1',
#                                      ifelse(HW_first_trimester==0 &
#                                             HW_second_trimester==0 &
#                                             HW_third_trimester==0, '0',
#                                NA)),
#         HW_3rd_trim_vs_not_exposed=as.factor(HW_3rd_trim_vs_not_exposed))
# 
# bevn_eco <- bevn_eco %>%
#   mutate(GR_3rd_trim_vs_not_exposed=ifelse(GR_first_trimester==0 &
#                                             GR_second_trimester==0 &
#                                             GR_third_trimester==1, '1',
#                                     ifelse(GR_first_trimester==0 &
#                                            GR_second_trimester==0 &
#                                            GR_third_trimester==0, '0',
#                                 NA)),
#          GR_3rd_trim_vs_not_exposed=as.factor(GR_3rd_trim_vs_not_exposed))
# 
# bevn_eco <- bevn_eco %>%
#   mutate(flu_3rd_trim_vs_not_exposed=ifelse(flu_first_trimester==0 &
#                                             flu_second_trimester==0 &
#                                             flu_third_trimester==1, '1',
#                                       ifelse(flu_first_trimester==0 &
#                                              flu_second_trimester==0 &
#                                              flu_third_trimester==0, '0',
#                                                   NA)),
#          flu_3rd_trim_vs_not_exposed=as.factor(flu_3rd_trim_vs_not_exposed))
# 
# bevn_eco <- bevn_eco %>%
#   mutate(covid_3rd_trim_vs_not_exposed=ifelse(COVID_first_trimester==0 &
#                                               COVID_second_trimester==0 &
#                                               COVID_third_trimester==1, '1',
#                                        ifelse(COVID_first_trimester==0 &
#                                               COVID_second_trimester==0 &
#                                               COVID_third_trimester==0, '0',
#                                                    NA)),
#          covid_3rd_trim_vs_not_exposed=as.factor(covid_3rd_trim_vs_not_exposed))

# 2023.09.14
# exposure : all combinations explored 
# bevn_eco <- bevn_eco %>%
#   mutate(HW_all_trim_comb=ifelse(HW_first_trimester==1 &
#                                  HW_second_trimester==0 &
#                                  HW_third_trimester==0, '1',
#                            ifelse(HW_first_trimester==0 &
#                                   HW_second_trimester==1 &
#                                   HW_third_trimester==0, '2',
#                             ifelse(HW_first_trimester==0 &
#                                    HW_second_trimester==0 &
#                                    HW_third_trimester==1, '3', 
#                      0))),
#          HW_all_trim_comb=as.factor(HW_all_trim_comb))
# 
# bevn_eco <- bevn_eco %>%
#   mutate(GR_all_trim_comb=ifelse(GR_first_trimester==1 &
#                                  GR_second_trimester==0 &
#                                  GR_third_trimester==0, '1',
#                            ifelse(GR_first_trimester==0 &
#                                   GR_second_trimester==1 &
#                                   GR_third_trimester==0, '2',
#                             ifelse(GR_first_trimester==0 &
#                                    GR_second_trimester==0 &
#                                    GR_third_trimester==1, '3', 
#                             ifelse(GR_first_trimester==1 &
#                                    GR_second_trimester==1 &
#                                    GR_third_trimester==0, '1+2',
#                             ifelse(GR_first_trimester==1 &
#                                   GR_second_trimester==0 &
#                                    GR_third_trimester==1, '1+3',
#                             ifelse(GR_first_trimester==0 &
#                                    GR_second_trimester==1 &
#                                    GR_third_trimester==1, '2+3',
#                              ifelse(GR_first_trimester==1 &
#                                     GR_second_trimester==1 &
#                                     GR_third_trimester==1, 'all',
#                         0))))))),
#          GR_all_trim_comb=as.factor(GR_all_trim_comb))
# 
# bevn_eco <- bevn_eco %>%
#   mutate(flu_all_trim_comb=ifelse(flu_first_trimester==1 &
#                                   flu_second_trimester==0 &
#                                   flu_third_trimester==0, '1',
#                             ifelse(flu_first_trimester==0 &
#                                   flu_second_trimester==1 &
#                                   flu_third_trimester==0, '2',
#                              ifelse(flu_first_trimester==0 &
#                                     flu_second_trimester==0 &
#                                     flu_third_trimester==1, '3', 
#                              ifelse(flu_first_trimester==1 &
#                                     flu_second_trimester==1 &
#                                     flu_third_trimester==0, '1+2',
#                              ifelse(flu_first_trimester==1 &
#                                     flu_second_trimester==0 &
#                                     flu_third_trimester==1, '1+3',
#                              ifelse(flu_first_trimester==0 &
#                                     flu_second_trimester==1 &
#                                     flu_third_trimester==1, '2+3',
#                              ifelse(flu_first_trimester==1 &
#                                     flu_second_trimester==1 &
#                                     flu_third_trimester==1, 'all',
#                             0))))))),
#          flu_all_trim_comb=as.factor(flu_all_trim_comb))
# 
# bevn_eco <- bevn_eco %>%
#   mutate(covid_all_trim_comb=ifelse(COVID_first_trimester==1 &
#                                     flu_second_trimester==0 &
#                                     flu_third_trimester==0, '1',
#                              ifelse(COVID_first_trimester==0 &
#                                     COVID_second_trimester==1 &
#                                      COVID_third_trimester==0, '2',
#                               ifelse(COVID_first_trimester==0 &
#                                      COVID_second_trimester==0 &
#                                      COVID_third_trimester==1, '3', 
#                               ifelse(COVID_first_trimester==1 &
#                                     COVID_second_trimester==1 &
#                                     COVID_third_trimester==0, '1+2',
#                               ifelse(COVID_first_trimester==1 &
#                                      COVID_second_trimester==0 &
#                                      COVID_third_trimester==1, '1+3',
#                                ifelse(COVID_first_trimester==0 &
#                                       COVID_second_trimester==1 &
#                                       COVID_third_trimester==1, '2+3',
#                                ifelse(COVID_first_trimester==1 &
#                                       COVID_second_trimester==1 &
#                                       COVID_third_trimester==1, 'all',
#                          0))))))),
#          covid_all_trim_comb=as.factor(covid_all_trim_comb))



# First and last trimesters. T. Rioux 20230920
# # Great Recession
# bevn_eco <- bevn_eco %>%
#   mutate(GR_trim_of_exposure=ifelse(GR_first_trimester==1 &
#                                     GR_last_trimester==0,
#                                       'first',
#                               ifelse(GR_first_trimester==0 &
#                                      GR_last_trimester==1, 'last',
#                                     '0'
#                                            )),
#          GR_trim_of_exposure=as.factor(GR_trim_of_exposure))

# Flu pandemic
# bevn_eco <- bevn_eco %>%
#   mutate(flu_trim_of_exposure=ifelse(flu_first_trimester==1 &
#                                       flu_last_trimester==0, 'first',
#                                      ifelse(flu_first_trimester==0 &
#                                               flu_last_trimester==1, 'last',
#                                           '0'
#                                             )),
#          flu_trim_of_exposure=as.factor(flu_trim_of_exposure))

# # Heatwave
# bevn_eco <- bevn_eco %>%
#   mutate(HW_trim_of_exposure=ifelse(HW_first_trimester==1, 'first',
#                             ifelse(HW_last_trimester==1, 'last',
#                               '0')),
#          HW_trim_of_exposure=as.factor(HW_trim_of_exposure))
