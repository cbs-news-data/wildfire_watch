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

#create list of fires of interest
fires_of_interest <- c("33 Incident Fire"
  )

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
if (safe_download("https://incidents.fire.ca.gov/umbraco/api/IncidentApi/GeoJsonList?inactive=true", temp_file)) {
  file.rename(temp_file, original_file)
}

# Download active federal wildfire perimeters
safe_download("https://services3.arcgis.com/T4QMspbfLg3qTGWY/ArcGIS/rest/services/WFIGS_Interagency_Perimeters_Current/FeatureServer/0/query?where=0%3D0&outFields=*&returnGeometry=true&f=pgeojson", perimeters_file)

# Function to convert milliseconds from UTC to POSIX date
ms_to_date <- function(ms, t0 = "1970-01-01", timezone = "America/Los_Angeles") {
  as.POSIXct(ms / 1000, origin = t0, tz = timezone)
}


# Load and process federal wildfire perimeters
nfis_raw <- st_read(perimeters_file, quiet = TRUE)

nfis_perimeters <- if (nrow(nfis_raw) == 0) {
  
  # empty sf with expected columns
  st_sf(
    fed_fire_id = character(),
    name = character(),
    state = character(),
    county = character(),
    started = as.Date(character()),
    perimeter_updated = as.Date(character()),
    data_updated = as.Date(character()),
    acres_burned = numeric(),
    percent_contained = numeric(),
    type = character(),
    fire_behavior = character(),
    fire_cause = character(),
    latitude = numeric(),
    longitude = numeric(),
    geometry = st_sfc(crs = st_crs(nfis_raw))
  )
  
} else {
  
  nfis_raw %>%
    select(
      fed_fire_id = attr_UniqueFireIdentifier,
      name = poly_IncidentName,
      state = attr_POOState,
      county = attr_POOCounty,
      started = attr_FireDiscoveryDateTime,
      perimeter_updated = poly_DateCurrent,
      data_updated = attr_ModifiedOnDateTime_dt,
      acres_burned = poly_GISAcres,
      percent_contained = attr_PercentContained,
      type = attr_IncidentTypeCategory,
      fire_behavior = attr_FireBehaviorGeneral,
      fire_cause = attr_FireCause,
      latitude = attr_InitialLatitude,
      longitude = attr_InitialLongitude,
      geometry
    ) %>%
    mutate(
      acres_burned = round(acres_burned),
      across(c(started, perimeter_updated, data_updated), ms_to_date),
      days_burning = as.integer(difftime(Sys.Date(), started, units = "days")),
      days_sinceupdate = as.integer(difftime(Sys.Date(), data_updated, units = "days"))
    ) %>%
    filter(acres_burned > min_acres_burned,
           days_sinceupdate < federal_update_threshold)
}

# Load and process California state wildfire data
calfire_raw <- st_read(original_file, quiet = TRUE)

calfire_activefires <- if (nrow(calfire_raw) == 0) {
  
  tibble(
    name = character(),
    state = character(),
    county = character(),
    location = character(),
    type = character(),
    latitude = numeric(),
    longitude = numeric(),
    started = as.Date(character()),
    perimeter_updated = as.Date(character()),
    data_updated = as.Date(character()),
    acres_burned = numeric(),
    percent_contained = numeric(),
    url = character(),
    days_burning = integer(),
    days_sinceupdate = integer()
  ) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
} else {
  
  calfire_raw %>%
    mutate(state = "CA") %>%
    select(
      name,
      state,
      county = County,
      location = Location,
      type = Type,
      latitude = Latitude,
      longitude = Longitude,
      started = Started,
      perimeter_updated = Updated,
      data_updated = Updated,
      acres_burned = AcresBurned,
      percent_contained = PercentContained,
      url = Url
    ) %>%
    mutate(
      across(c(acres_burned, percent_contained), as.numeric),
      percent_contained = replace_na(percent_contained, 0),
      days_burning = as.integer(difftime(Sys.Date(), started, units = "days")),
      days_sinceupdate = as.integer(difftime(Sys.Date(), data_updated, units = "days"))
    ) %>%
    filter(acres_burned > min_acres_burned,
           days_sinceupdate < calfire_update_threshold)
}

# Merge California and federal data
fires <- bind_rows(nfis_perimeters, calfire_activefires) %>%
  mutate(
    name = str_to_title(str_replace_all(trimws(name), "  ", " ")),
    state = str_replace(state, "US-", ""),
    name = ifelse(!str_detect(name, "Fire$"), paste0(name, " Fire"), name)
  )

fires_flags <- fires %>% 
  mutate(fire_of_interest = ifelse(name %in% fires_of_interest, TRUE, FALSE))
  

# Save merged data as GeoJSON
st_write(fires, save_file, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)

message("Wildfire data successfully processed and saved to: ", save_file)

# Save as csv
write_csv(fires,"data/wildfires_save.csv")

message("Wildfire data successfully processed and saved to: data/wildfires_save.csv")
