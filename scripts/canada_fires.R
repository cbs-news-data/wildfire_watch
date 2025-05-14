# Libraries
library(readr)
library(dplyr)
library(stringr)
library(sf)
library(janitor)
library(lubridate)
library(tidyr)

#https://cwfis.cfs.nrcan.gc.ca/datamart/metadata/activefires

#https://cwfis.cfs.nrcan.gc.ca/downloads/activefires/activefires.csv

download.file("https://cwfis.cfs.nrcan.gc.ca/downloads/activefires/activefires.csv", "data/canada_wildfires_raw.csv")

canada_wf <- read.csv("data/canada_wildfires_raw.csv")

min_acres_burned = 49

canada_wf_clean <- canada_wf %>% 
  mutate(stage_of_control = trimws(stage_of_control)) %>% 
  mutate(timezone = trimws(timezone)) %>% 
  filter(stage_of_control == "OC") %>% #filter for "out of control" stage
  mutate(acres = round((hectares*2.471),digits=0)) %>% 
  filter(acres > min_acres_burned) %>% 
  mutate(startdate = trimws(startdate)) %>% 
  mutate(startdate = ymd_hms(startdate)) %>% 
  mutate(days_burning = as.integer(difftime(Sys.Date(), startdate, units = "days"))) %>% 
  mutate(startdate_clean = format(startdate, format = "%B %e at %I:%M %p"),
         startdate_clean = gsub("AM", "a.m.", gsub("PM", "p.m.", startdate_clean)),
         startdate_clean = gsub("^([A-Za-z]+)\\s+", "\\1 ", startdate_clean),
         startdate_clean = paste0(as.character(startdate_clean), " ", timezone)
         )

canada_wf_clean_sf <- st_as_sf(canada_wf_clean, coords = c("lon", "lat"), crs = 4326)

st_write(canada_wf_clean_sf, "data/canada_wildfires_clean.geojson", driver = "GeoJSON", delete_dsn = TRUE)
