# Libraries
library(stringr)

# Get updated date/time in "America/Los_Angeles" timezone and format it
updated_datetime <- format(Sys.time(), "%B %d, %Y at %I:%M %p %Z", tz = "America/Los_Angeles")

# Replace " 0" with a space to clean up the format
updated_datetime_pretty <- str_replace_all(updated_datetime, " 0", " ")

# Replace AM/PM with a.m./p.m.
updated_datetime_pretty <- str_replace_all(updated_datetime_pretty, "AM", "a.m.")
updated_datetime_pretty <- str_replace_all(updated_datetime_pretty, "PM", "p.m.")

# Save to CSV
write.csv(data.frame(updated_datetime_pretty), "data/updated_datetime_df.csv", row.names = FALSE)