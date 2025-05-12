# Libraries
library(readr)
library(dplyr)
library(stringr)
library(sf)
library(janitor)
library(lubridate)
library(tidyr)

# Configuration variables
# Adjust these based on seasonal conditions
min_acres_burned = 49
federal_update_threshold = 60  
calfire_update_threshold = 30  

# Define file paths
temp_file <- "data/temp.geojson"
original_file <- "data/calfire_activefires.geojson"
perimeters_file <- "data/active_perimeters.geojson"
save_file <- "data/wildfires_combined.geojson"

# Function to safely download files
safe_download <- function(url, dest) {
  status <- try(download.file(url, dest, quiet = TRUE), silent = TRUE)
  if (inherits(status, "try-error")) {
    message("Failed to download: ", url)
    return(FALSE)
  }
  TRUE
}

# Download and update CalFire data
if (safe_download("https://www.fire.ca.gov/umbraco/api/IncidentApi/GeoJsonList?inactive=true", temp_file)) {
  file.rename(temp_file, original_file)
}

# Download active federal wildfire perimeters
safe_download("https://services3.arcgis.com/T4QMspbfLg3qTGWY/ArcGIS/rest/services/WFIGS_Interagency_Perimeters_Current/FeatureServer/0/query?where=0%3D0&outFields=*&returnGeometry=true&f=pgeojson", perimeters_file)

# Function to convert milliseconds from UTC to POSIX date
ms_to_date <- function(ms, t0 = "1970-01-01", timezone = "America/Los_Angeles") {
  as.POSIXct(ms / 1000, origin = t0, tz = timezone)
}

# Load and process federal wildfire perimeters
nfis_perimeters <- st_read(perimeters_file, quiet = TRUE) %>%
  select(
    fed_fire_id = attr_UniqueFireIdentifier,
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
    geometry
  ) %>%
  mutate(
    across(c(started, updated), ms_to_date),
    days_burning = as.integer(difftime(Sys.Date(), started, units = "days")),
    days_sinceupdate = as.integer(difftime(Sys.Date(), updated, units = "days"))
  ) %>%
  filter(acres_burned > min_acres_burned, days_sinceupdate < federal_update_threshold) %>% 
  mutate(latitude = case_when(fed_fire_id == "2025-MN2QS-001729" ~ 47.2898754221127,
                              TRUE ~ latitude)) %>% 
  mutate(longitude = case_when(fed_fire_id == "2025-MN2QS-001729" ~ -91.8411746756444,
                              TRUE ~ longitude)) #manually fix Camp House fire lat/long in MN

# Load and process California state wildfire data
calfire_activefires <- st_read(original_file, quiet = TRUE) %>%
  mutate(state = "CA") %>%
  select(
    name = Name,
    state,
    county = County,
    location = Location,
    type = Type,
    latitude = Latitude,
    longitude = Longitude,
    started = Started,
    updated = Updated,
    acres_burned = AcresBurned,
    percent_contained = PercentContained,
    url = Url
  ) %>%
  mutate(
    across(c(acres_burned, percent_contained), as.numeric),
    percent_contained = replace_na(percent_contained, 0),
    days_burning = as.integer(difftime(Sys.Date(), started, units = "days")),
    days_sinceupdate = as.integer(difftime(Sys.Date(), updated, units = "days"))
  ) %>%
  filter(acres_burned > min_acres_burned, days_sinceupdate < calfire_update_threshold)

# Merge California and federal data
fires <- bind_rows(nfis_perimeters, calfire_activefires) %>%
  mutate(
    name = str_to_title(str_replace_all(trimws(name), "  ", " ")),
    state = str_replace(state, "US-", ""),
    name = ifelse(!str_detect(name, "Fire$"), paste0(name, " Fire"), name)
  )

# Save merged data as GeoJSON
st_write(fires, save_file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)

message("Wildfire data successfully processed and saved to: ", save_file)

# Save as csv
write_csv(fires,"data/wildfires_save.csv")