library(xml2)
library(lubridate)

# Read the Apple Health export XML file
xml_file <- "/Users/bertneef/Library/Mobile Documents/com~apple~CloudDocs/Bert/Running/apple_health_export/export.xml"

cat("Reading XML file...\n")
health_data <- read_xml(xml_file)

cat("Extracting all workouts...\n")
all_workouts <- xml_find_all(health_data, "//Workout")
workout_types <- xml_attr(all_workouts, "workoutActivityType")
source_names <- xml_attr(all_workouts, "sourceName")

# Find Nike Run Club workouts
nike_filter <- (workout_types == "HKWorkoutActivityTypeRunning") &
  grepl("Nike", source_names, ignore.case = TRUE)

nike_workouts <- all_workouts[nike_filter]
cat(paste("Found", length(nike_workouts), "Nike Run Club running workouts\n"))

# Get details of Nike workouts to find one >5km
cat("\nNike Run Club workouts:\n")
for (i in 1:min(10, length(nike_workouts))) {
  w <- nike_workouts[[i]]

  # Get distance from WorkoutStatistics
  dist_node <- xml_find_first(w, './/WorkoutStatistics[@type="HKQuantityTypeIdentifierDistanceWalkingRunning"]')
  distance <- if (length(dist_node) > 0 && !is.na(dist_node)) {
    as.numeric(xml_attr(dist_node, "sum"))
  } else {
    NA
  }

  start_date <- xml_attr(w, "startDate")
  end_date <- xml_attr(w, "endDate")

  cat(sprintf("%d. Date: %s, Distance: %.2f km\n", i, start_date, distance))
}

# Pick the first Nike workout >5km (let's say workout #1 based on the grep output we saw earlier)
cat("\n=== Testing HR extraction for first recent Nike workout ===\n")
test_workout <- nike_workouts[[1]]

start_date <- xml_attr(test_workout, "startDate")
end_date <- xml_attr(test_workout, "endDate")

cat(sprintf("Workout: %s to %s\n", start_date, end_date))

# Convert to POSIXct
start_time <- ymd_hms(start_date, quiet = TRUE)
end_time <- ymd_hms(end_date, quiet = TRUE)

cat(sprintf("Parsed times: %s to %s\n", start_time, end_time))

# Now try different methods to extract HR records

cat("\n--- Method 1: Direct XPath query ---\n")
hr_records_1 <- xml_find_all(health_data, '//Record[@type="HKQuantityTypeIdentifierHeartRate"]')
cat(paste("Found", length(hr_records_1), "HR records with direct query\n"))

cat("\n--- Method 2: Find all Records first, then filter ---\n")
all_records <- xml_find_all(health_data, '//Record')
cat(paste("Found", length(all_records), "total Record elements\n"))

if (length(all_records) > 0) {
  # Sample first few records to see their types
  cat("Sample of first 10 record types:\n")
  for (i in 1:min(10, length(all_records))) {
    record_type <- xml_attr(all_records[[i]], "type")
    cat(sprintf("  %d. %s\n", i, record_type))
  }

  # Now filter for HR
  cat("\nFiltering for HR records...\n")
  record_types <- xml_attr(all_records, "type")
  hr_filter <- record_types == "HKQuantityTypeIdentifierHeartRate"
  hr_records_2 <- all_records[hr_filter]
  cat(paste("Found", length(hr_records_2), "HR records after filtering\n"))

  if (length(hr_records_2) > 0) {
    cat("\nExtracting HR data for our test workout...\n")

    # Get timestamps and values
    hr_starts_raw <- xml_attr(hr_records_2, "startDate")
    hr_values_raw <- xml_attr(hr_records_2, "value")
    hr_sources <- xml_attr(hr_records_2, "sourceName")

    # Convert timestamps
    hr_starts <- ymd_hms(hr_starts_raw, quiet = TRUE)
    hr_values <- as.numeric(hr_values_raw)

    # Filter to our workout time window
    matches <- !is.na(hr_starts) & !is.na(hr_values) &
      hr_starts >= start_time & hr_starts <= end_time

    matching_records <- sum(matches)
    cat(paste("Found", matching_records, "HR records matching workout time window\n"))

    if (matching_records > 0) {
      matching_values <- hr_values[matches]
      matching_sources <- hr_sources[matches]

      cat(sprintf("\nHR Statistics:\n"))
      cat(sprintf("  Average: %.1f bpm\n", mean(matching_values)))
      cat(sprintf("  Min: %.0f bpm\n", min(matching_values)))
      cat(sprintf("  Max: %.0f bpm\n", max(matching_values)))
      cat(sprintf("  Count: %d readings\n", matching_records))

      cat("\nSources of HR data:\n")
      print(table(matching_sources))

      cat("\nFirst 5 HR readings:\n")
      first_five <- head(which(matches), 5)
      for (idx in first_five) {
        cat(sprintf("  %s: %.0f bpm (source: %s)\n",
                    hr_starts[idx], hr_values[idx], hr_sources[idx]))
      }
    }
  }
}

cat("\n=== Test complete ===\n")
