## Compile DNR chemistry data ##
## script author : Lienne Sethna ##

## 1. housekeeping

rm(list=ls())
#install librarian package first if not installed
librarian::shelf(readxl,tidyverse,supportR,janitor)

getwd()
setwd("/Users/lsethna/Documents/GitHub/MPCA_Rainy_Vermilion_HABs_Risk_Assessment")

## 2. read in DNR data

dnr_1 <- read.csv("raw_data/MNDNR data/MN_DNR_FAW_chemisty_1964_2013.csv")
dnr_2 <- read.csv("raw_data/MNDNR data/MN_DNR_FAW_chemisty_2014_2016.csv")
dnr_3 <- read.csv("raw_data/MNDNR data/MN_DNR_FAW_chemisty_2016_2025.csv")
dplyr::glimpse(dnr_3)
supportR::diff_check(colnames(dnr_2),colnames(dnr_3))

## 3. merge DNR data together by time?

dnr_1_v2 <- dnr_1 %>% 
  #convert everything to lower case
  janitor::clean_names() %>%
  select(yearcoll,datecol,waterbody,dowid,ph:fefiltppb) %>%
  mutate(across(ph:fefiltppb, as.numeric),
         #date formatted as yymmdd, except for year 2000 and months 1-9 (single digits)
         #need to add zeros to create uniform date pattern?
         datecol= str_pad(datecol,width=6,side="left",pad="0"),
         #get month and day strings
         month = substr(datecol,3,4),
         day = substr(datecol,5,6),
         date = paste(yearcoll,month,day,sep="-"),
         datecol = as.Date(date)) %>%
  select(-c(yearcoll,month,day,date,contains("cod"))) %>% #remove "code" (sometimes just "cod") from analytes
  pivot_longer(cols=!c(datecol,waterbody,dowid),names_to="analyte",values_to="result") %>%
  mutate(unit = str_extract(analyte, "ppb|ppm|umho"), #extract the unit from anywhere in the string
         analyte = str_remove(analyte, "ppb|ppm|umho"), #remove that unit from the analyte name, preserving prefixes and suffixes (like tr/ph)
         source = "MN_DNR_FAW_chemisty_1964_2013.csv")
range(dnr_1_v2$datecol)
unique(dnr_1_v2$analyte)

dnr_2_v2 <- dnr_2 %>% janitor::clean_names() %>%
  select(date_sampled,dnr_id,dow,analyte,result,unit) %>%
  #filter out date/time and criteria from analytes
  filter(!str_detect(analyte, "Date/Time|Criteria")) %>%
  #change all to lower case and remove spaces
  mutate(
    analyte = analyte %>% 
      str_to_lower() %>% 
      str_remove_all(" ")
  ) %>%
  rename(datecol=date_sampled,waterbody=dnr_id,dowid=dow) %>%
  mutate(datecol = as.Date(datecol,format="%m/%d/%Y %H:%M"),
         result = as.numeric(result),
         source = "MN_DNR_FAW_chemisty_2014_2016.csv")
range(dnr_2_v2$datecol)
glimpse(dnr_2_v2)

dnr_3_v2 <- dnr_3 %>% janitor::clean_names() %>%
  select(sampled_on,dow,chemical_name,result_value,result_unit) %>%
  rename(datecol=sampled_on,dowid=dow,analyte=chemical_name,result=result_value,unit=result_unit) %>%
  mutate(datecol = as.Date(datecol,format="%m/%d/%y"),
         analyte = analyte %>% 
           str_to_lower() %>% 
           str_remove_all(" "),
         source = "MN_DNR_FAW_chemisty_2016_2025.csv")
range(dnr_3_v2$datecol)
unique(dnr_3_v2$analyte)

dnr_all <- dnr_1_v2 %>% full_join(dnr_2_v2) %>% full_join(dnr_3_v2)
dplyr::glimpse(dnr_all)
length(unique(dnr_all$dowid))
range(dnr_all$datecol)

#harmonize analytes
unique(dnr_all$analyte)

dnr_all <- dnr_all %>% mutate(analyte = case_when(analyte == "ph" ~ "pH",
                                                  analyte %in% c("tp", "totalphosphorus", "totalphosphorus(lowlevel)") ~ "TP",
                                                  analyte %in% c("sulf", "sulfate") ~ "sulfate",
                                                  analyte %in% c("chlatr", "chlorophylla(trichromaticcalculation)") ~ "chla_tricalc",
                                                  analyte %in% c("chlaph", "chlorophylla(pheophytincalculation)") ~ "chla_pheacalc",
                                                  analyte %in% c("tds", "totaldissolvedsolids") ~ "TDS",
                                                  analyte %in% c("talk", "totalalkalinity") ~ "total_alkalinity",
                                                  analyte %in% c("no2_3","no2n", "no3n") ~ "nox_n",
                                                  analyte == "cond" ~ "conductivity",
                                                  T ~ analyte))

write.csv(dnr_all, file="raw_data/MNDNR data/compiled_DNR_chemistry_1964_2025.csv")

## 4. create master list of  wids, number of observations, year, and lake name within source
dnr_site_list <- dnr_all %>% group_by(dowid) %>%
  summarize(n_samples = n(),
            n_analytes = length(unique(analyte)),
            year_range = paste(year(min(datecol)),year(max(datecol)),sep="-")
            )

