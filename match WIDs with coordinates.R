## Get lat/lons by WID from geospatial commons ##
## script author : Lienne Sethna ##

## 1. housekeeping

rm(list=ls())
#install librarian package first if not installed
librarian::shelf(readxl,tidyverse,supportR,janitor,sf,ggmap,ggspatial)

getwd()
setwd("/Users/lsethna/Documents/GitHub/MPCA_Rainy_Vermilion_HABs_Risk_Assessment")

## 2. Read in data

wids <- read_excel("raw_data/Vermillion_RainyHW_lake WIDS.xlsx") %>%
  select(wid,dowknum...5) %>%
  rename(dowlknum = dowknum...5)
dplyr::glimpse(wids)

data <- read.csv("raw_data/equis_lakebrowser_summary.csv") %>% select(!X)
glimpse(data)

# read in Geospatial Commons "hydro features"
lakes <- sf::read_sf("Geospatial_commons/shp_water_lake_bathymetry/lake_bathymetric_outline.shp") %>%
  sf::st_transform("epsg:4326") %>%
  janitor::clean_names() %>% 
  mutate(dowlknum = as.numeric(dowlknum))
dplyr::glimpse(lakes)

# read in and filter HUC-8s
huc8 <- sf::read_sf("Geospatial_commons/HUC8_CONUS/HUC8_US.shp") %>% 
  sf::st_transform("epsg:4326")

## 3. Filter hydro features to wids
vermilion_rainy_features <- wids %>% left_join(data) %>% left_join(lakes)
glimpse(vermilion_rainy_features)
unique(vermilion_rainy_features$huc8)
#covert back to sf object
vermilion_rainy_sf <- vermilion_rainy_features %>%
  st_as_sf() %>%
  filter(!st_is_empty(geometry))

ggplot(data = vermilion_rainy_sf) +
  geom_sf(aes(fill = in_equis_lake_browser), color = "gray30", linewidth = 0.2) +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 11)
  )
