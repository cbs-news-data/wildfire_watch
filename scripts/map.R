library(readr)
library(dplyr)
library(stringr)
library(leaflet)
library(sf)
library(janitor)
library(lubridate)
library(tidyr)

# Define file paths
temp_file <- "data/temp.geojson"
original_file <- "data/calfire_activefires.geojson"
perimeters_file <- "data/active_perimeters.geojson"
save_file <- "data/wildfires_combined.geojson"

# Download and update CalFire data
download_status <- try(download.file("https://www.fire.ca.gov/umbraco/api/IncidentApi/GeoJsonList?inactive=true", temp_file))
if (!inherits(download_status, "try-error")) file.rename(temp_file, original_file)

# Download active federal wildfire perimeters
download.file("https://services3.arcgis.com/T4QMspbfLg3qTGWY/ArcGIS/rest/services/WFIGS_Interagency_Perimeters_Current/FeatureServer/0/query?where=0%3D0&outFields=*&returnGeometry=true&f=pgeojson", perimeters_file)

# Function to convert milliseconds from UTC
ms_to_date <- function(ms, t0 = "1970-01-01", timezone = "America/New_York") {
  as.POSIXct(ms / 1000, origin = t0, tz = timezone)
}

# Load and process federal wildfire perimeters
nfis_perimeters <- st_read(perimeters_file) %>% 
  select(fed_fire_id = attr_UniqueFireIdentifier, 
         name = poly_IncidentName, 
         state = attr_POOState,
         county = attr_POOCounty,
         started = attr_FireDiscoveryDateTime, 
         updated = poly_DateCurrent, 
         acres_burned = attr_IncidentSize,
         percent_contained = attr_PercentContained,
         type = attr_IncidentTypeCategory,
         fire_behavior = attr_FireBehaviorGeneral, 
         fire_cause = attr_FireCause,
         latitude = attr_InitialLatitude,
         longitude = attr_InitialLongitude,
         geometry) %>% 
  mutate(across(c(started, updated), ~ ms_to_date(.))) %>%
  mutate(days_burning = as.integer(difftime(Sys.Date(), started, units = "days")),
         days_sinceupdate = as.integer(difftime(Sys.Date(), updated, units = "days"))) %>%
  filter(acres_burned > 99 & days_sinceupdate < 60)

# Load and process California state wildfire data
calfire_activefires <- st_read(original_file) %>%
  mutate(state = "CA") %>%
  select(name = Name, state, county = County, 
         location = Location, type = Type, 
         latitude = Latitude, longitude = Longitude, 
         started = Started, updated = Updated, 
         acres_burned = AcresBurned, percent_contained = PercentContained, 
         url = Url) %>%
  mutate(across(c(acres_burned, percent_contained), as.numeric, .names = "clean_{col}"),
         percent_contained = replace_na(percent_contained, 0),
         days_burning = as.integer(difftime(Sys.Date(), started, units = "days")),
         days_sinceupdate = as.integer(difftime(Sys.Date(), updated, units = "days"))) %>%
  filter(acres_burned > 99 & days_sinceupdate < 30)

# Merge California and federal data
fires <- bind_rows(nfis_perimeters, calfire_activefires) %>%
  mutate(name = str_to_title(name),
         name = str_replace_all(name, "  ", " "),
         name = trimws(name),
         state = str_replace(state, "US-", ""))


# Append "Fire" to fires without that

# Filter out fires with both a polygon and a point, keeping only the polygon


# Convert to GeoJSON and save
st_write(fires, save_file, driver = "GeoJSON", delete_dsn = TRUE)