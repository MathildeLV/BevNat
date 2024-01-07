# COVID exposure
#loading data file for hospitalization cases per week
data.covid.org <- data.covid.org %>%
  mutate(year_week=as.factor(datum_dboardformated)) 
data.covid.org <- separate(data.covid.org, col=year_week, into=c('year', 'week'), sep='-', remove = FALSE)  
  
  data.covid.org <- data.covid.org %>%
  mutate(year=as.numeric(year),
         week=as.numeric(week)) 
    
  data.covid.org$first_day_of_week <- as.Date(paste(data.covid.org$year, "01", "01", sep = "-"), format = "%Y-%m-%d") +
    lubridate::weeks(data.covid.org$week - 1)
  
  # Extract the ISO year and month from the start date
  data.covid.org <- data.covid.org%>% 
  mutate(iso_year= lubridate::year(first_day_of_week),
         iso_month=lubridate::month(first_day_of_week))
  
  # Format the result as "YYYY-MM"
  data.covid.org <- data.covid.org %>%
    mutate(iso_month_format=sprintf("%04d-%02d", data.covid.org$iso_year, data.covid.org$iso_month),
           day= 01,
           Y_M_1stday = as.Date(paste(iso_year, iso_month, day, sep='-')))
  summary(data.covid.org$Y_M_1stday)
  # bevn_eco$birth_Y_M_1stday_num <- as.numeric(bevn_eco$birth_Y_M_1stday)
  
  covid_monthly_hosp_national <- data.covid.org %>%
    group_by(geoRegion, Y_M_1stday) %>%
    summarise(monthly_hospitalizations = sum(entries, na.rm = TRUE)) %>%
    filter(geoRegion=="CH") %>%
    select(Y_M_1stday, monthly_hospitalizations) %>%
    ungroup()
  covid_monthly_hosp_national <- covid_monthly_hosp_national %>%
  select(-geoRegion)
  
  ## Merge hospitalization cases to bevn_eco, so that each month of pregnancy
  # is matched with the number of COVID hospitalizations that happened during
  # that month
  bevn_eco <- left_join(bevn_eco, covid_monthly_hosp_national, by = c(
    "month_1" = "Y_M_1stday"))
  bevn_eco <- bevn_eco %>%
    rename(monthly_hospitalizations_month1=monthly_hospitalizations) 
  
  bevn_eco <- left_join(bevn_eco, covid_monthly_hosp_national, by = c(
    "month_2" = "Y_M_1stday"))
  bevn_eco <- bevn_eco %>%
    rename(monthly_hospitalizations_month2=monthly_hospitalizations)
  
  bevn_eco <- left_join(bevn_eco, covid_monthly_hosp_national, by = c(
    "month_3" = "Y_M_1stday"))
  bevn_eco <- bevn_eco %>%
    rename(monthly_hospitalizations_month3=monthly_hospitalizations)
  
  bevn_eco <- left_join(bevn_eco, covid_monthly_hosp_national, by = c(
    "month_4" = "Y_M_1stday"))
  bevn_eco <- bevn_eco %>%
    rename(monthly_hospitalizations_month4=monthly_hospitalizations)
  
  bevn_eco <- left_join(bevn_eco, covid_monthly_hosp_national, by = c(
    "month_5" = "Y_M_1stday"))
  bevn_eco <- bevn_eco %>%
    rename(monthly_hospitalizations_month5=monthly_hospitalizations)
  
  bevn_eco <- left_join(bevn_eco, covid_monthly_hosp_national, by = c(
    "month_6" = "Y_M_1stday"))
  bevn_eco <- bevn_eco %>%
    rename(monthly_hospitalizations_month6=monthly_hospitalizations)
  
  bevn_eco <- left_join(bevn_eco, covid_monthly_hosp_national, by = c(
    "month_7" = "Y_M_1stday"))
  bevn_eco <- bevn_eco %>%
    rename(monthly_hospitalizations_month7=monthly_hospitalizations)
  
  bevn_eco <- left_join(bevn_eco, covid_monthly_hosp_national, by = c(
    "month_8" = "Y_M_1stday"))
  bevn_eco <- bevn_eco %>%
    rename(monthly_hospitalizations_month8=monthly_hospitalizations)
  
  bevn_eco <- left_join(bevn_eco, covid_monthly_hosp_national, by = c(
    "month_9" = "Y_M_1stday"))
  bevn_eco <- bevn_eco %>%
    rename(monthly_hospitalizations_month9=monthly_hospitalizations)
  
  bevn_eco <- left_join(bevn_eco, covid_monthly_hosp_national, by = c(
    "month_10" = "Y_M_1stday"))
  bevn_eco <- bevn_eco %>%
    rename(monthly_hospitalizations_month10=monthly_hospitalizations)
  
  bevn_eco <- left_join(bevn_eco, covid_monthly_hosp_national, by = c(
    "month_11" = "Y_M_1stday"))
  bevn_eco <- bevn_eco %>%
    rename(monthly_hospitalizations_month11=monthly_hospitalizations)
  
  bevn_eco <- left_join(bevn_eco, covid_monthly_hosp_national, by = c(
    "birth_Y_M_1stday" = "Y_M_1stday"))
  bevn_eco <- bevn_eco %>%
    rename(monthly_hospitalizations_birthmonth=monthly_hospitalizations)
  
  # set the variables to numeric
  bevn_eco <- bevn_eco%>%
    mutate(monthly_hospitalizations_month1=as.numeric(monthly_hospitalizations_month1), 
           monthly_hospitalizations_month2=as.numeric(monthly_hospitalizations_month2),
           monthly_hospitalizations_month3=as.numeric(monthly_hospitalizations_month3),
           monthly_hospitalizations_month4=as.numeric(monthly_hospitalizations_month4),
           monthly_hospitalizations_month5=as.numeric(monthly_hospitalizations_month5),
           monthly_hospitalizations_month6=as.numeric(monthly_hospitalizations_month6),
           monthly_hospitalizations_month7=as.numeric(monthly_hospitalizations_month7),
           monthly_hospitalizations_month8=as.numeric(monthly_hospitalizations_month8),
           monthly_hospitalizations_month9=as.numeric(monthly_hospitalizations_month9),
           monthly_hospitalizations_month10=as.numeric(monthly_hospitalizations_month10),
           monthly_hospitalizations_month11=as.numeric(monthly_hospitalizations_month11),
           monthly_hospitalizations_birthmonth=as.numeric(monthly_hospitalizations_birthmonth)
    )
  
 # replace missing values by 0
  bevn_eco <- bevn_eco %>%
    mutate(monthly_hospitalizations_month1 = replace(monthly_hospitalizations_month1, (is.na(monthly_hospitalizations_month1)), 0),
    monthly_hospitalizations_month2 = replace(monthly_hospitalizations_month2, (is.na(monthly_hospitalizations_month2)), 0),
    monthly_hospitalizations_month3 = replace(monthly_hospitalizations_month3, (is.na(monthly_hospitalizations_month3)), 0),
    monthly_hospitalizations_month4 = replace(monthly_hospitalizations_month4, (is.na(monthly_hospitalizations_month4)), 0),
    monthly_hospitalizations_month5 = replace(monthly_hospitalizations_month5, (is.na(monthly_hospitalizations_month5)), 0),
    monthly_hospitalizations_month6 = replace(monthly_hospitalizations_month6, (is.na(monthly_hospitalizations_month6)), 0),
    monthly_hospitalizations_month7 = replace(monthly_hospitalizations_month7, (is.na(monthly_hospitalizations_month7)), 0),
    monthly_hospitalizations_month8 = replace(monthly_hospitalizations_month8, (is.na(monthly_hospitalizations_month8)), 0),
    monthly_hospitalizations_month9 = replace(monthly_hospitalizations_month9, (is.na(monthly_hospitalizations_month9)), 0),
    monthly_hospitalizations_month10= replace(monthly_hospitalizations_month10, (is.na(monthly_hospitalizations_month10)), 0),
    monthly_hospitalizations_month11 = replace(monthly_hospitalizations_month11, (is.na(monthly_hospitalizations_month11)), 0),
    monthly_hospitalizations_birthmonth = replace(monthly_hospitalizations_birthmonth, (is.na(monthly_hospitalizations_birthmonth)), 0))

  
  
  ## summing all the covid cases, adj for GA duration
   # normalized value for covid_hosp_by_pregn_month (ignoring the missing cases)
  # GA_weeks_present <- bevn_eco %>%
  #   filter(!is.na(GA_weeks))
  
bevn_eco <- bevn_eco %>%
    mutate(total_covid_hosp=ifelse(!is.na(GA_month), rowSums(select(., starts_with("monthly_hospitalizations_"))),NA),
           covid_hosp_1st_trim=ifelse(!is.na(GA_month),
                                      monthly_hospitalizations_month1+monthly_hospitalizations_month2+monthly_hospitalizations_month3, 
                                      NA),
           covid_hosp_last_trim=case_when(
             GA_month==4 ~ (monthly_hospitalizations_month3 + monthly_hospitalizations_month4 + monthly_hospitalizations_birthmonth),
             GA_month==5 ~ (monthly_hospitalizations_month4+ monthly_hospitalizations_month5+ monthly_hospitalizations_birthmonth),
            GA_month==6 ~ (monthly_hospitalizations_month5+ monthly_hospitalizations_month6+ monthly_hospitalizations_birthmonth),
            GA_month==7 ~ (monthly_hospitalizations_month6+ monthly_hospitalizations_month7+ monthly_hospitalizations_birthmonth),
            GA_month==8 ~ (monthly_hospitalizations_month7+ monthly_hospitalizations_month8+ monthly_hospitalizations_birthmonth),
            GA_month==9 ~ (monthly_hospitalizations_month8+ monthly_hospitalizations_month9+ monthly_hospitalizations_birthmonth),
            GA_month==10 ~ (monthly_hospitalizations_month9+ monthly_hospitalizations_month10+ monthly_hospitalizations_birthmonth),
            GA_month==11 ~ (monthly_hospitalizations_month10+ monthly_hospitalizations_month11+ monthly_hospitalizations_birthmonth))
)
  table(bevn_eco$total_covid_hosp, useNA = "always")
  table(bevn_eco$covid_hosp_1st_trim, useNA = "always")
  table(bevn_eco$covid_hosp_last_trim, useNA = "always")
  
  bevn_eco <- bevn_eco %>%
    mutate(covid_hosp_by_pregn_month=total_covid_hosp/GA_month) %>%
    mutate(covid_hosp_by_pregn_month_cat=cut(covid_hosp_by_pregn_month, breaks=c(0,1640,4930)),
           covid_hosp_by_pregn_month_cat2 = case_when (
             covid_hosp_by_pregn_month_cat=="(1.64e+03,4.93e+03]" ~"highly exposed",
             covid_hosp_by_pregn_month_cat=="(0,1.64e+03]" ~"moderately exposed",
             covid_hosp_by_pregn_month==0 ~"never exposed"
           ))

  
  bevn_eco$covid_hosp_by_pregn_month_cat2 <- factor(bevn_eco$covid_hosp_by_pregn_month_cat2,
                                                         levels=c('never exposed',
                                                                  'moderately exposed',
                                                                  'highly exposed'))

  test <- bevn_eco %>%
    select("GA_month", "birth_Y_M_1stday",
           "month_1", "month_4", "month_5", "month_6", "month_7", "month_8",
           "month_9", "month_10", "month_11",
           "monthly_hospitalizations_month1", "monthly_hospitalizations_month2", "monthly_hospitalizations_month3",
           "monthly_hospitalizations_month4",  "monthly_hospitalizations_month5",  "monthly_hospitalizations_month6", 
           "monthly_hospitalizations_month7",  "monthly_hospitalizations_month8",  "monthly_hospitalizations_month9", 
           "monthly_hospitalizations_month10",  "monthly_hospitalizations_month11",  "monthly_hospitalizations_birthmonth",
           "total_covid_hosp",  
           "covid_hosp_1st_trim",
           "covid_hosp_last_trim"
    )