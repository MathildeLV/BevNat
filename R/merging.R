##{merging the ecological var files}
# anth$MSRegion <- as.numeric(anth$MSRegion)
  gem <- gem %>%
  mutate(MS_Reg = as.numeric(MS_Reg))
# gem$MS_Reg <- as.numeric(gem$MS_Reg)
summary(gem$MS_Reg)

anth <- anth %>%
  rename(MS_Reg=MSRegion) %>% #like this MS region variables are named the same (MS_Reg) in both gem and anth datasets.
  mutate(MS_Reg= as.numeric(MS_Reg))

summary(anth$MS_Reg)

eco <- anth %>%
  full_join(gem)



# Merging bevn and eco file
summary(eco$comm16)
summary(bevn$Mutter..Wohngemeinde...Wohnstaat) ##1-6910 for commune if resident inside Switz <br> 8201-8703 if resident outside of Switz <br>


bevn$Mutter..Wohngemeinde...Wohnstaat <- as.numeric(bevn$Mutter..Wohngemeinde...Wohnstaat) 
bevn <-  rename(bevn, com=Mutter..Wohngemeinde...Wohnstaat) 
eco <-  rename(eco, com=comm16) 
#like this commune variables are named the same (com) in both bevn and eco datasets.

summary(bevn$com)
summary(eco$com)

# to check Gemeindenummer

bevn_test <- bevn %>%
  full_join(eco)

com_eco <- bevn_test %>%
  filter( is.na(Statistikjahr)) %>%
  dplyr::select(com)

com_eco

com_bevn <- bevn_test %>%
  filter( is.na(MS_Reg)) %>%
  dplyr::select(com) %>%
  distinct(com) %>%
  filter(com<8000)
com_bevn


#recoding Gemeindenummer
eco2 <- eco %>%
  dplyr::mutate(com =as.character(com),
         com = dplyr::recode(com, "36"="292",
                      "42"="292",
                      "44"="292",
                      "133"="295",
                      "132"="295",
                      "142"="293",
                      "140"="293",
                      "134"="293",
                      "217"="294",
                      "222"="294",
                      "334"="329",
                      "416"="409",
                      "624"="608",
                      "625"="616",
                      "664"="304",
                      "865"="872",
                      "875"="872",
                      "878"="872",
                      "873"="889",
                      "874"="889",
                      "876"="889",
                      "881"="879",
                      "937"="939",
                      "996"="981",
                      "1022"="1030",
                      "1126"="1123",
                      "1130"="1151",
                      "1204"="1214",
                      "2004"="2054",
                      "2005"="2055",
                      "2009"="2044",
                      "2010"="2055",
                      "2015"="2054",
                      "2033"="2054",
                      "2034"="2054",
                      "2039"="2054",
                      "2047"="2044",
                      "2049"="2054",
                      "2052"="2054",
                      "2111"="2117",
                      "2116"="2117",
                      "2171"="2238",
                      "2179"="2183",
                      "2185"="2237",
                      "2189"="2238",
                      "2213"="2237",
                      "2221"="2237",
                      "2225"="2238",
                      "2243"="2254",
                      "2279"="2254",
                      "2283"="2254",
                      "2291"="2306",
                      "2298"="2299",
                      "2302"="2306",
                      "2310"="2299",
                      "2423"="2430",
                      "2429"="2430",
                      "2498"="2499",
                      "3503"="3668",
                      "3521"="3544",
                      "3522"="3544",
                      "3611"="3981",
                      "3616"="3981",
                      "3691"="3714",
                      "3693"="3714",
                      "3694"="3714",
                      "3703"="3715",
                      "3705"="3715",
                      "3707"="3715",
                      "3708"="3715",
                      "3833"="3832",
                      "3836"="3832",
                      "3926"="3901",
                      "3941"="3901",
                      "4113"="4104",
                      "4114"="4095",  
                      "4272"="4281",
                      "5004"="5002",
                      "5005"="5002",
                      "5006"="5002",
                      "5007"="5002",
                      "5008"="5002",
                      "5012"="5002",
                      "5013"="5002",
                      "5014"="5002",
                      "5015"="5002",
                      "5018"="5002",
                      "5019"="5002",
                      "5095"="5399",
                      "5102"="5399",
                      "5105"="5399",
                      "5129"="5399",
                      "5135"="5399",
                      "5178"="5239",
                      "5202"="5239",
                      "5213"="5239",
                      "5222"="5239",
                      "5282"="5002",
                      "5283"="5287",
                      "5284"="5287",
                      "5285"="5287",
                      "5286"="5287",
                      "5421"="5656",
                      "5432"="5422",
                      "5478"="5656",
                      "5494"="5656",
                      "5500"="5656",
                      "5513"="5511",
                      "5625"="5656",
                      "5644"="5656",
                      "5662"="5675",
                      "5666"="5675",
                      "5668"="5675",
                      "5672"="5675",
                      "5686"="5675",
                      "5751"="5749",
                      "5915"="5749",
                      "6031"="6037",
                      "6036"="6037",
                      "6055"="6077",
                      "6064"="6077",
                      "6073"="6077",
                      "6074"="6077",
                      "6075"="6077",
                      "6081"="6266",
                      "6132"="6136",
                      "6234"="6253",
                      "6241"="6254",
                      "6242"="6253",
                      "6243"="6253",
                      "6244"="6253",
                      "6249"="6254",
                      "6250"="6254",
                      "6402"="6417",
                      "6407"="6458",
                      "6409"="6417",
                      "6410"="6417",
                      "6411"="6417",
                      "6412"="6458",
                      "6414"="6417",
                      "6415"="6417",
                      "6431"="6436",
                      "6485"="6458",
                      "6705"="6730",
                      "6720"="6708",
                      "6728"="6708",
                      "6803"="6809")) %>%
  

  #attribute mean(meanssep) for recoded Gemeindenr and attribute mean(Alt_mean) for recoded Gemeindenr 
  dplyr::group_by(com) %>%
  dplyr::mutate(mean_ssep2 = mean(mean_ssep)) %>%
  dplyr::mutate(mean_Alt_mean2 = mean(Alt_Mean)) %>%
  dplyr::distinct(com, .keep_all = TRUE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(com = as.numeric(com))
eco2


bevn <- bevn %>%
  mutate(com =as.numeric(com))


bevn_eco <- bevn %>%
  full_join(eco2) 


bevn_eco%>%
  filter( is.na(Statistikjahr)) %>%
  dplyr::select(com)

tab <- bevn_eco %>%
  filter(mean_ssep!=mean_ssep2) 



round(prop.table(table(bevn_eco$Language, useNA="always"))*100,2)