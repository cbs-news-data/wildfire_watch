library(readr)
library(dplyr)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sf)
library(htmlwidgets)
library(htmltools)
library(janitor)
library(lubridate)

# Stripped-down version of a longer-term project
# Focusing first here just on fire locations, perimeters and basic data points on each

# Get live state and fed data
# If file is not live, functions will not stop; latest live data will build map

temp_file <- "data/temp.geojson"
original_file <- "data/calfire_activefires.geojson"

# Try downloading the file to a temporary file
download_status <- try(download.file("https://www.fire.ca.gov/umbraco/api/IncidentApi/GeoJsonList?inactive=true", temp_file))

# If download is successful, move the temporary file to replace the original file
if (!inherits(download_status, "try-error")) {
  file.rename(temp_file, original_file)
}

# Get active FEDERAL WILDFIRE PERIMETERS from NFIS for both perimeters and points
#try(download.file("https://stg-arcgisazurecdataprod3.az.arcgis.com/exportfiles-2532-182272/WFIGS_Interagency_Perimeters_Current_-2078219868963267088.geojson?sv=2018-03-28&sr=b&sig=zsgucf%2FdBtaFv%2BVO58iWkKVXOCdAxWZfvWa6wUQrGvc%3D&se=2024-11-13T21%3A01%3A11Z&sp=r",
#                  "data/active_perimeters.geojson"))
try(download.file("https://services3.arcgis.com/T4QMspbfLg3qTGWY/ArcGIS/rest/services/WFIGS_Interagency_Perimeters_Current/FeatureServer/0/query?where=0%3D0&fullText=&objectIds=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&returnEnvelope=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&collation=&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnTrueCurves=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson&token=",
                  "data/active_perimeters.geojson"))

# saved function to convert the milliseconds from UTC 
ms_to_date = function(ms, t0="1970-01-01", timezone) {
  sec = ms / 1000
  as.POSIXct(sec, origin=t0, tz=timezone)
}

# Read in federal fire perimeters and streamling cols
nfis_perimeters <- st_read("data/active_perimeters.geojson") %>% 
  select(attr_UniqueFireIdentifier, poly_IncidentName, attr_POOState, attr_POOCounty, 
         attr_FireDiscoveryDateTime, poly_DateCurrent, attr_IncidentSize,attr_PercentContained,
         attr_IncidentTypeCategory, attr_FireBehaviorGeneral, attr_FireCause,
         attr_InitialLatitude,attr_InitialLongitude) %>% 
  rename(fed_fire_id = attr_UniqueFireIdentifier, 
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
         longitude = attr_InitialLongitude)

#nfis_perimeters <- nfis_perimeters %>% 
#  mutate(
#    started = with_tz(dmy_hms(attr_FireDiscoveryDateTime, tz = "GMT"), tzone = "America/New_York"),
#    updated = with_tz(dmy_hms(poly_DateCurrent, tz = "GMT"), tzone = "America/New_York")
#  )

# Simplify the geometry
# First, repair invalid geometries
nfis_perimeters <- st_make_valid(nfis_perimeters)
# Remove any records in nfis_perimeters with fewer than 100 acres_burned
nfis_perimeters <- nfis_perimeters %>% filter(acres_burned > 49)
# Adjust dTolerance as needed. Smaller values = less simplification
# simplified_nfis_perimeters <- st_simplify(nfis_perimeters, dTolerance = 0.2)

# clean numbers into fed_fires file
fed_fires <- nfis_perimeters %>% 
  st_drop_geometry() %>%
  mutate(source="NFIS") %>%
  mutate_at(vars(started, updated), ms_to_date, t0 = "1970-01-01", timezone = "America/New_York") %>% 
  mutate(days_burning = floor(difftime(Sys.time(), started, units = "days")), 
         days_sinceupdate = round(difftime(Sys.time(), updated, units = "days"), 1)) %>% 
  filter(acres_burned > 49 & days_sinceupdate < 90 | days_sinceupdate < 90)

# Standardize fire name and state columns
fed_fires <- fed_fires %>% 
  mutate(name = str_to_title(name),
         name = paste0(name, " Fire"),
         name = gsub("  ", " ", name),
         name = trimws(name),
         state = gsub("US-", "", state))

# Strip to needed cols and filter to fires remaining in points file only
nfis_perimeters <- nfis_perimeters %>% 
  select(name, fed_fire_id, geometry) %>% 
  filter(fed_fire_id %in% fed_fires$fed_fire_id)

# Standardize fire name column
nfis_perimeters$name <- str_to_title(nfis_perimeters$name)
nfis_perimeters$name <- paste0(nfis_perimeters$name, " Fire")

# Load California state geojson as sf
calfire_activefires <- st_read("data/calfire_activefires.geojson")

# Simplify, standardize version of California Fires from CalFire's active list
try(
  cal_fires <- calfire_activefires %>%
    mutate(state="CA") %>%
    select(1,25,7,8,15,14,13,4,3,9,10,17) %>%
    st_drop_geometry() %>%
    mutate(source="CalFire")
)
try(
  names(cal_fires) <- c("name", "state", "county", 
                        "location", "type", "latitude", "longitude", 
                        "started", "updated", "acres_burned", "percent_contained",
                        "info_url","source")
)

# clean numeric fields
try(
  cal_fires$acres_burned <- round(as.numeric(cal_fires$acres_burned),0)
)
try(
  cal_fires$percent_contained <- as.numeric(cal_fires$percent_contained)
)
# if percent contained is NA, replace with numeric zero
try(
  cal_fires$percent_contained[is.na(cal_fires$percent_contained)] <- 0
)
# calculating fields for time passed elements in popups and for filtering old fires
try(
  cal_fires$days_burning <- floor(difftime(Sys.Date(),cal_fires$started, units="days"))+1
)
try(
  cal_fires$days_sinceupdate <- floor(difftime(Sys.Date(),cal_fires$updated, units="days"))+1
)
# OPEN WORK: Verify and solve the time zones for the math for
# both the California and federal files' time stamps
try(
  cal_fires$name <- trimws(cal_fires$name)
)
# filter out small fires and old fires not updated for more than a week
# except for leaving in very new fires
try(
  cal_fires <- cal_fires %>%
    filter(acres_burned>49 & days_sinceupdate<30 |
             days_sinceupdate<30)
)

# If a fire in fed_fires in california is also in cal_fires, add the latitude and longitude from cal_fires file
try(
  fed_fires <- fed_fires %>%
    left_join(cal_fires %>% select(name,state,latitude,longitude), by=c("name","state")) %>%
    mutate(latitude = ifelse(is.na(latitude.x), latitude.y, latitude.x),
           longitude = ifelse(is.na(longitude.x), longitude.y, longitude.x)) %>%
    select(-latitude.x, -longitude.x, -latitude.y, -longitude.y)
)

# Merge and clean CA and national
# Temporarily reduce California file
try(
  cal_fires_unique <- cal_fires %>%
    filter(!cal_fires$name %in% fed_fires$name)
)

fires <- fed_fires
try(fires <- bind_rows(fires,cal_fires_unique))
# try(rm(cal_fires_unique))
# Add full state name
states <- as.data.frame(cbind(state.abb,state.name)) %>% janitor::clean_names()
fires <- left_join(fires,states,by=c("state"="state_abb"))

# manually add lat long to Airport Fire in Orange County
#fires$latitude <- ifelse(fires$name=="Airport Fire", 33.665728, fires$latitude)
#fires$longitude <- ifelse(fires$name=="Airport Fire", -117.567802, fires$longitude)


# Save latest merged fire points file as csv
write_csv(fires,"data/wildfires_save.csv")


# Remove fires without lat longs so we can map
# Manual validation = all tiny <1ac and all <10ac
fires <- fires %>% filter(!is.na(latitude) & !is.na(longitude))
# Create flag for active vs. not for icons; 8-day cutoff
fires$active <- if_else(fires$days_sinceupdate<30,"Yes","No")
# filter out fires that are not active or smaller than 100 acres
fires <- fires %>% filter(active=="Yes") %>% filter(acres_burned>49)
# count number of fires we're tracking
fires_count <- fires %>% st_drop_geometry() %>% count()

# Properly project
fires <- st_as_sf(fires, coords = c("longitude", "latitude"), 
                  crs = 4326)

# Build popup labels, separate for fires and for perimeters

fireLabel <- paste(sep = "",
                   paste("<font size='5' font color=#820415 style='font-family: \"PublicoHeadline\";'><b>",(fires$name),"</font size></b><br><font size='2' style='font-family: \"proxima-nova\", sans-serif;'>",fires$county," County,",fires$state_name,"<br><br>"),
                   paste("<font size='2'>Started ", ifelse(fires$days_burning < 2, "about <b>1</b> day ago<br>", paste("<b>", fires$days_burning, "</b> days ago<br>")), "<b>", prettyNum(fires$acres_burned, big.mark = ","), "</b> acres burned <br></font>"),
                   paste("<b>",ifelse(is.na(fires$percent_contained),"</b>Percent contained not reported",paste(sep="",fires$percent_contained,"</b>","% contained"))),
                   paste("<br><br>"),
                   paste("<font size='1'><i>Last updated: ", format(as.POSIXct(fires$updated, format = "%Y-%m-%d %H:%M"), "%b %d at %I:%M %p"), "</i></font>")
)

# Create temporary perimeter label
perimeterLabel <- paste(nfis_perimeters$name," current perimeter")

# Create the fire icons
fireIcons <- awesomeIcons(
  icon = "fire",
  iconColor = "#A5091E",
  library = 'glyphicon',
  markerColor = "white")
# options include ion-flame, ion-fireball, fa-fire


# Styling 

tag.map.title <- tags$style(HTML("

 @font-face {
    font-family: 'PublicoHeadline';
    src: url('https://www.cbsnews.com/fly/bundles/cbsnewscontent/fonts/PublicoHeadline-Black/PublicoHeadline-Black.woff2') format('woff2');
  }
  
  .leaflet-top {
    width: 100%;
  }

  .leaflet-control.map-title {
    text-align: center;
    margin: auto;
    width: 100%;
  }
  
.header{
    width: 100%;
    position: relative;
    height: 100px;
    background: white;
    text-align: left;
  }
  
  .headline {
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    color: black;
    width: 100%;
    font-family: \"PublicoHeadline\";
    text-align: left;
  }
  
 .headline h1 {
    font-weight: bold;
    font-size: 32px;
    color: black;
    width: 100%;
    font-family: \"PublicoHeadline\";
  }
  
  .headline h3 {
    font-size: 16px;
    width: 100%;
    font-family: \"proxima-nova\", sans-serif;
  }
  
  .h1, .h2, .h3, h1, h2, h3 {
    margin-top: 12px;
    margin-bottom: 0px;
}
  
.subheadline a {
    color: #BE0000;
    font-weight: bold;
  }
  
  .leaflet-popup-content-wrapper {
    padding: 5px;
    text-align: left;
    background-color: #FAEDE9 !important;
    border-radius: 5px;
  }

  .leaflet-popup-tip{
    background: #FAEDE9 !important;
  }

@media only screen and (max-width: 380px) {
  .h1, .h2, .h3, h1, h2, h3 {
    margin-top: 6px;
    margin-bottom: 0px;
}
}

"))

headerhtml <- tags$div(tag.map.title, HTML(
  paste(
    sep = "",
    "<head><meta name='viewport' content='width=device-width, initial-scale=1.0'><link rel=\"stylesheet\" href=\"https://use.typekit.net/vtp4rxj.css\"></head>
  <div class='header'>
    <div class='headline'>
      <h1>Current wildfires</h1>
      <h3>We are currently tracking <b style='color: #A5091E'>",
    fires_count,
    "</b> wildfires of more than 50 acres in the U.S. Click or hover over an icon to see more information about each fire.<br><br></h3>
    </div>
  </div>"
  ))
)

# New wildfire base map include fires, smoke and hotspots
wildfire_map <- leaflet(nfis_perimeters, options = leafletOptions(zoomControl = FALSE, scrollWheelZoom = FALSE)) %>%
  addControl(position = "topleft", html = headerhtml, className="map-title") %>%
  setView(-118.3,34.2, zoom = 8.5) %>%
  #  addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$CartoDB.Voyager) %>%
  addPolygons(data = nfis_perimeters, 
              color = "darkred",
              popup = perimeterLabel,
              weight = 1.5) %>%
  addAwesomeMarkers(data = fires,
                    popup = fireLabel,
                    popupOptions = popupOptions(keepInView = T, 
                                                autoPanPaddingTopLeft = c(100, 120)),
                    icon = fireIcons) %>%
  addLayersControl(options = layersControlOptions(collapsed = FALSE),
                   position = 'topright') %>%
  htmlwidgets::onRender("function(el, x) {
          L.control.zoom({ position: 'topleft'}).addTo(this)
      }") %>%
  htmlwidgets::onRender("
      function(el, x) {
          document.getElementsByClassName('leaflet-control-layers')[0].style.display = 'none';
      }")
wildfire_map


# Write to html
saveWidget(wildfire_map, 'docs/cbs_wildfire_tracking_map.html', title = "CBS News Live Wildfire Tracking Map")


#get updated date/time
updated_datetime <- format(as.POSIXct(Sys.time()), format = "%B %d, %Y %I:%M %p %Z", tz = "America/Los_Angeles")

updated_datetime_char <- as.character(updated_datetime)

updated_datetime_pretty <- updated_datetime_char %>% 
  str_replace_all(" 0", " ")

updated_datetime_df <- data.frame(updated_datetime_pretty)

write.csv(updated_datetime_df, "data/updated_datetime_df.csv", row.names = FALSE)
