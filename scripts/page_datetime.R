# Load necessary library
library(stringr)

# Get current time in UTC and adjust for Pacific Standard Time (always PST, not PDT)
updated_datetime <- format(as.POSIXct(Sys.time(), tz = "UTC"), 
                           "%B %d, %Y at %I:%M %p")

# Replace " 0" with a space for formatting
updated_datetime_pretty <- str_replace_all(updated_datetime, " 0", " ")

# Replace AM/PM with a.m./p.m.
updated_datetime_pretty <- str_replace_all(updated_datetime_pretty, "AM", "a.m.")
updated_datetime_pretty <- str_replace_all(updated_datetime_pretty, "PM", "p.m.")

# Append PST manually
updated_datetime_pretty <- paste(updated_datetime_pretty, "PST")

# Save to CSV
write.csv(data.frame(updated_datetime_pretty), "data/updated_datetime_df.csv", row.names = FALSE)