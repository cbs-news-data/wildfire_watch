# Libraries
library(dplyr)
library(readr)
library(stringr)
library(DatawRappr)
library(dotenv)
library(lubridate)

# Load the .env file
tryCatch({
  load_dot_env()
}, error = function(e) {
  # Do nothing
})
dw_api_key <- Sys.getenv("DW_API_KEY")

# NATIONAL CONTAINMENT TABLE
library(dplyr)
library(stringr)
library(lubridate)

fires_fordatawrappertable <- read.csv("data/wildfires_save.csv") %>%
  filter(updated >= Sys.Date() - 30) %>%
  mutate(county_state = paste0(county, ", ", state)) %>%
  select(name,
         county_state,
         started,
         updated,
         acres_burned,
         percent_contained) %>%
  mutate(
    started = format(as.Date(started), format = "%b. %d, %Y"),
    updated = format(as.Date(updated), format = "%b. %d, %Y")
  ) %>%
  mutate(percent_contained = case_when(
    is.na(as.character(percent_contained)) ~ "Not available",
    TRUE ~ as.character(percent_contained)
  )) %>%
  rename(
    Name = name,
    `County` = county_state,
    Started = started,
    Updated = updated,
    `Acres burned` = acres_burned,
    `% contained` = percent_contained
  )

# Get date for Datawrapper
# Get current date and time in UTC
current_datetime_utc <- Sys.time()

# Convert UTC to Eastern Time
current_datetime_eastern <- with_tz(current_datetime_utc, "America/New_York")

# Round the datetime to the nearest hour
rounded_datetime <- round_date(current_datetime_eastern, "hour")

# Format the date and time
formatted_datetime <- format(rounded_datetime, "%b. %e, %Y at %l %p ET.")

# Make the Datawrapper
datawrapper_auth(api_key = dw_api_key)
dw_data_to_chart(fires_fordatawrappertable, "9zNRi", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "9zNRi",
  api_key = dw_api_key,
  annotate = paste(
    "Note: Data as of", formatted_datetime),
  folderId = "250056"
)

# Publish the chart
dw_publish_chart(chart_id = "9zNRi")

#CALIFORNIA CONTAINMENT TABLE
# create a copy of the same table but for california only, filtering the records that end with the string ", CA"
fires_fordatawrappertable_cali <- fires_fordatawrappertable %>% 
  filter(grepl(", CA", County))
# Then manually drop the individual duplicate where the fire is name "Hurst (2025-Calfd-003294) Fire"
fires_fordatawrappertable_cali <- fires_fordatawrappertable_cali %>% 
  filter(!grepl("2025-Calfd-003294", Name))

# Make the Datawrapper
datawrapper_auth(api_key = dw_api_key)
dw_data_to_chart(fires_fordatawrappertable_cali, "QuBuz", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "QuBuz",
  api_key = dw_api_key,
  annotate = paste(
    "Note: Data as of", formatted_datetime),
  folderId = "250056"
)

# Publish the chart
dw_publish_chart(chart_id = "QuBuz")
