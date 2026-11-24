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

#get hucs for Rainy+Vermilion
vermilion_rainy_huc_list <- data %>% filter(!is.na(huc8)) %>% select(huc8) %>% 
  #add zeros to make HUC8 8 digits
  mutate(huc8 = str_pad(huc8,width=8,side="left",pad="0"),
         huc8 = as.character(huc8)) %>%
  distinct() %>% pull()

# read in Geospatial Commons "hydro features"
lakes <- sf::read_sf("Geospatial_commons/shp_water_lake_bathymetry/lake_bathymetric_outline.shp") %>%
  sf::st_transform("epsg:4326") %>%
  janitor::clean_names() %>% 
  mutate(dowlknum = as.numeric(dowlknum))
dplyr::glimpse(lakes)

# read in and filter HUC-8s
huc8 <- sf::read_sf("Geospatial_commons/HUC8_CONUS/HUC8_US.shp") %>% 
  sf::st_transform("epsg:4326")
dplyr::glimpse(huc8)
length(unique(huc8$HUC8))

vermilion_rainy_huc8 <- huc8 %>% filter(HUC8 %in% vermilion_rainy_huc_list)

## 3. Filter hydro features to wids
vermilion_rainy_features <- wids %>% left_join(data) %>% left_join(lakes)
glimpse(vermilion_rainy_features)
unique(vermilion_rainy_features$huc8)
#covert back to sf object
vermilion_rainy_sf <- vermilion_rainy_features %>%
  st_as_sf() %>%
  filter(!st_is_empty(geometry))

# filter out islands and convert lake polygons to interior points
vermilion_rainy_pts <- vermilion_rainy_sf %>%
  filter(island == "N" | is.na(island)) %>%
  st_point_on_surface()

# plot as points (use 'color' instead of 'fill' for standard point shapes)
ggplot() +
  geom_sf(data = vermilion_rainy_huc8, fill = "gray96", color = "gray40", linewidth = 0.4) +
  geom_sf(
    data = vermilion_rainy_pts,
    aes(color = in_equis_lake_browser),
    size = 2,
    alpha = 0.85
  ) +
  geom_sf_label(
    data = vermilion_rainy_huc8,
    aes(label = HUC8),
    size = 3.5,
    fontface = "bold",
    fill = alpha("white", 0.85),
    label.size = 0.25
  ) +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 11)
  )
