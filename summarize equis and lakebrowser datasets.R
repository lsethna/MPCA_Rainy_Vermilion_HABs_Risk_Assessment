## Create Rainy-Vermilion lake reference sheet to list lakes, available data, and sources ##
## script author : Lienne Sethna ##

## 1. housekeeping

rm(list=ls())
#install librarian package first if not installed
librarian::shelf(readxl,tidyverse,supportR,janitor)

getwd()
setwd("/Users/lsethna/Documents/GitHub/MPCA_Rainy_Vermilion_HABs_Risk_Assessment")

## 2. Read in data
equis <- read_excel("raw_data/rain_lake_study.xlsx")
lakebrowser <- read_excel("raw_data/UMNLakeBrowser_Daily_RainyHW_Vermillion.xlsx",sheet=2)
lakebrowser_meta <- read_excel("raw_data/UMNLakeBrowser_Daily_RainyHW_Vermillion.xlsx")

dplyr::glimpse(equis)
dplyr::glimpse(lakebrowser)

## 3. clean data
equis_v2 <- equis %>% janitor::clean_names() %>%
  mutate(dowid = paste0(stringr::str_split(equis$WID,"-")[[1]][1],stringr::str_split(equis$WID,"-")[[1]][2]),
         #change all chemical names to lower case
         chemical_name = stringr::str_to_lower(chemical_name),
         #except for pH
         chemical_name = case_when(chemical_name=="ph"~"pH",
                                   T~chemical_name)) %>%
  select(date,wid,dowid,loc_name,latitude,longitude,huc8,chemical_name,depth,result_numeric,result_unit) %>%
  rename(analyte=chemical_name,depth_m=depth,result=result_numeric,unit=result_unit)
dplyr::glimpse(equis_v2)
unique(equis_v2$analyte)
length(unique(equis_v2$wid))

lakebrowser_v2 <- lakebrowser %>% 
  mutate(Field = case_when(Field=="2m0iA4" ~ "CDOM",
                           Field=="2m0iCL" ~ "chla",
                           Field=="2m0iSD" ~ "clarity")) %>%
  mutate(Date = as.Date(as.character(Date),format="%Y%m%d"))
glimpse(lakebrowser_v2)
unique(lakebrowser_v2$Field)

## 4. Create summary table of which WIDs are in each dataset
equis_wids <- equis_v2 %>% group_by(wid,huc8) %>%
  summarize(equis_year_range = paste(year(min(date)),year(max(date)),sep="-"),
            equis_n_years = lubridate::year(max(date))-lubridate::year(min(date)) + 1,
            equis_n_analytes = length(unique(analyte)),
            equis_n_obs = n())

lakebrowser_wids <- lakebrowser_v2 %>%
  group_by(wid) %>%
  summarize(lakebrowser_year_range = paste(year(min(Date)),year(max(Date)),sep="-"),
            lakebrowser_n_years = lubridate::year(max(Date))-lubridate::year(min(Date)) + 1,
            lakebrowser_n_analytes = length(unique(Field)),
            lakebrowser_n_obs = n())

supportR::diff_check(equis_wids$wid,lakebrowser_wids$wid)
length(which(equis_wids$wid %in% lakebrowser_wids$wid))

## 5. Merge EQuIS and LakeBrowser summaries together
equis_lakebrowser_summary <- equis_wids %>% full_join(lakebrowser_wids) %>%
  mutate(in_equis_lake_browser = case_when(!is.na(equis_year_range) & is.na(lakebrowser_year_range) ~ "EQuIS",
                                           is.na(equis_year_range) & !is.na(lakebrowser_year_range) ~ "LakeBrowser",
                                           !is.na(equis_year_range) & !is.na(lakebrowser_year_range) ~ "both",
                                           T ~ NA))
equis_lakebrowser_summary %>% group_by(in_equis_lake_browser) %>% summarize(n=n())

write.csv(equis_lakebrowser_summary,file="summarized datasets/equis_lakebrowser_summary.csv")

## 6. Filter to analytes common in DNR and lake browser?
#table of analytes and their count
equis_analytes <- equis_v2 %>% group_by(analyte) %>% summarize(n=n()) %>%
  #keep only anaytes with more than 1 observation
  filter(n>1) %>%
  #order by nobs
  arrange(n,desc=T)
equis_analytes$analyte

unique(dnr$analyte)

equis_v3 <- equis_v2 %>%
  filter(str_detect(analyte, "pH|alkalinity|conduct|chlorphyll|ammoni|
                    phosphorus|nitrogen|carbon|silic|calcium|magnesium|
                    solids|oxygen|transparency"))
unique(equis_v3$analyte)







