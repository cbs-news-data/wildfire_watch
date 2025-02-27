# LIBRARIES
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

# Configuration variables
# Adjust these based on seasonal conditions
min_acres_burned = 49

# Load data
fires_fordatawrappertable <- read.csv("data/wildfires_save.csv") %>%
  filter(updated >= Sys.Date() - 30, acres_burned > min_acres_burned) %>%
  mutate(
    county_state = paste0(county, ", ", state),
    started = format(as.Date(started), format = "%b. %d, %Y"),
    updated = format(as.Date(updated), format = "%b. %d, %Y"),
    percent_contained = case_when(
      is.na(as.character(percent_contained)) ~ "Not available",
      TRUE ~ as.character(percent_contained)
    )
  ) %>%
  select(
    Name = name,
    `County` = county_state,
    Started = started,
    Updated = updated,
    `Acres burned` = acres_burned,
    `% contained` = percent_contained
  )


# Generate timestamps for charts
formatted_datetime_national <- Sys.time() %>%
  with_tz("America/New_York") %>%
  round_date("hour") %>%
  format("%b. %e, %Y at %l %p EST.")

formatted_datetime_cali <- Sys.time() %>%
  with_tz("America/Los_Angeles") %>%
  round_date("hour") %>%
  format("%b. %e, %Y at %l %p PST.")

# Upload and publish national containment chart
datawrapper_auth(api_key = dw_api_key)
dw_data_to_chart(fires_fordatawrappertable, "9zNRi", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "9zNRi",
  api_key = dw_api_key,
  annotate = paste("Note: Data as of", formatted_datetime_national),
  folderId = "250056"
)

dw_publish_chart(chart_id = "9zNRi")

# Upload and publish California containment chart
# Filter only California wildfires
fires_fordatawrappertable_cali <- fires_fordatawrappertable %>%
  filter(grepl(", CA", County)) %>%
  filter(!grepl("2025-Calfd-003294", Name))  # Remove specific duplicate fire

# Upload California data
dw_data_to_chart(fires_fordatawrappertable_cali, "QuBuz", api_key = dw_api_key)

dw_edit_chart(
  chart_id = "QuBuz",
  api_key = dw_api_key,
  annotate = paste("Note: Data as of", formatted_datetime_cali),
  folderId = "250056"
)

dw_publish_chart(chart_id = "QuBuz")