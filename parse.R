library(xml2)
library(dplyr)
library(lubridate)

# Read the Apple Health export XML file
# Replace with your actual file path
xml_file <- "/Users/bertneef/Library/Mobile Documents/com~apple~CloudDocs/Bert/Running/apple_health_export/export.xml"
health_data <- read_xml(xml_file)

# Extract all workouts first
cat("Extracting all workouts...\n")
workouts <- xml_find_all(health_data, "//Workout")

cat(paste("Found", length(workouts), "total workouts\n"))

# Filter for running workouts
cat("Filtering for running workouts...\n")
activity_types <- xml_attr(workouts, "workoutActivityType")
running_filter <- activity_types == "HKWorkoutActivityTypeRunning"
workouts <- workouts[running_filter]

cat(paste("Found", length(workouts), "running workouts\n"))

# Check what attributes are available (inspect first workout)
cat("Checking available attributes...\n")
if (length(workouts) > 0) {
  sample_attrs <- xml_attrs(workouts[[1]])
  cat("Available attributes on first workout:\n")
  print(names(sample_attrs))
  cat("\n")
}

# Vectorized extraction of all attributes at once
cat("Extracting workout attributes...\n")
running_workouts <- data.frame(
  source_name = xml_attr(workouts, "sourceName"),
  source_version = xml_attr(workouts, "sourceVersion"),
  device = xml_attr(workouts, "device"),
  creation_date = xml_attr(workouts, "creationDate"),
  start_date = xml_attr(workouts, "startDate"),
  end_date = xml_attr(workouts, "endDate"),
  duration = as.numeric(xml_attr(workouts, "duration")),
  duration_unit = xml_attr(workouts, "durationUnit"),
  stringsAsFactors = FALSE
)

# Extract distance and energy from WorkoutStatistics child elements
cat("Extracting workout statistics (distance, energy)...\n")
distance_data <- sapply(workouts, function(w) {
  # Look for distance statistic
  dist_node <- xml_find_first(w, './/WorkoutStatistics[@type="HKQuantityTypeIdentifierDistanceWalkingRunning"]')
  if (length(dist_node) > 0 && !is.na(dist_node)) {
    return(c(
      distance = as.numeric(xml_attr(dist_node, "sum")),
      distance_unit = xml_attr(dist_node, "unit")
    ))
  }
  return(c(distance = NA_real_, distance_unit = NA_character_))
})

energy_data <- sapply(workouts, function(w) {
  # Look for energy statistic
  energy_node <- xml_find_first(w, './/WorkoutStatistics[@type="HKQuantityTypeIdentifierActiveEnergyBurned"]')
  if (length(energy_node) > 0 && !is.na(energy_node)) {
    return(c(
      energy = as.numeric(xml_attr(energy_node, "sum")),
      energy_unit = xml_attr(energy_node, "unit")
    ))
  }
  return(c(energy = NA_real_, energy_unit = NA_character_))
})

# Add distance and energy to dataframe
running_workouts$distance <- as.numeric(distance_data[1,])
running_workouts$distance_unit <- distance_data[2,]
running_workouts$energy <- as.numeric(energy_data[1,])
running_workouts$energy_unit <- energy_data[2,]

# Filter for Apple Workouts and Nike Run Club early
running_workouts <- running_workouts %>%
  filter(grepl("Watch|Workout|Nike", source_name, ignore.case = TRUE))

cat(paste("Filtered to", nrow(running_workouts), "workouts from Apple/Nike\n"))

# Only extract metadata for the filtered workouts
cat("Extracting heart rate data from Records...\n")

# Create index mapping for filtered workouts
source_filter <- grepl("Watch|Workout|Nike", xml_attr(workouts, "sourceName"), ignore.case = TRUE)
filtered_workout_nodes <- workouts[source_filter]

# Get workout times FIRST (before extracting HR records)
cat("Preparing workout time windows...\n")
workout_times <- data.frame(
  start = ymd_hms(xml_attr(filtered_workout_nodes, "startDate"), quiet = TRUE),
  end = ymd_hms(xml_attr(filtered_workout_nodes, "endDate"), quiet = TRUE)
)

# Find the overall time range of all workouts
min_workout_time <- min(workout_times$start, na.rm = TRUE)
max_workout_time <- max(workout_times$end, na.rm = TRUE)

cat(paste("Workout time range:", min_workout_time, "to", max_workout_time, "\n"))

# Only extract HR records within the workout time range (MUCH faster query)
cat("Finding heart rate records in workout time range...\n")
hr_records <- xml_find_all(health_data, '//Record[@type="HKQuantityTypeIdentifierHeartRate"]')

if (length(hr_records) > 0) {
  cat(paste("Found", length(hr_records), "total heart rate records\n"))
  
  # Extract HR record data - vectorized
  cat("Extracting and filtering HR values...\n")
  hr_starts_raw <- xml_attr(hr_records, "startDate")
  hr_values_raw <- xml_attr(hr_records, "value")
  
  # Convert dates only once
  hr_starts <- ymd_hms(hr_starts_raw, quiet = TRUE)
  
  # Filter to only HR records within workout time range
  time_filter <- hr_starts >= min_workout_time & hr_starts <= max_workout_time
  
  hr_data <- data.frame(
    value = as.numeric(hr_values_raw[time_filter]),
    start = hr_starts[time_filter],
    stringsAsFactors = FALSE
  )
  
  # Remove NAs
  hr_data <- hr_data[!is.na(hr_data$start) & !is.na(hr_data$value), ]
  
  cat(paste("Filtered to", nrow(hr_data), "HR records in workout time range\n"))
  
  if (nrow(hr_data) > 0) {
    # Sort once
    hr_data <- hr_data[order(hr_data$start), ]
    
    # Pre-convert to numeric timestamps for even faster comparison
    hr_timestamps <- as.numeric(hr_data$start)
    workout_starts <- as.numeric(workout_times$start)
    workout_ends <- as.numeric(workout_times$end)
    
    cat("Matching heart rate to workouts...\n")
    
    # Vectorize the matching using data.table-style approach
    avg_hr <- numeric(nrow(workout_times))
    max_hr <- numeric(nrow(workout_times))
    
    for (i in 1:nrow(workout_times)) {
      if (i %% 100 == 0) cat(paste0("Processing workout ", i, "/", nrow(workout_times), "\r"))
      
      # Find matching indices using numeric comparison (faster)
      matches <- hr_timestamps >= workout_starts[i] & hr_timestamps <= workout_ends[i]
      
      if (any(matches)) {
        matching_vals <- hr_data$value[matches]
        avg_hr[i] <- mean(matching_vals, na.rm = TRUE)
        max_hr[i] <- max(matching_vals, na.rm = TRUE)
      } else {
        avg_hr[i] <- NA_real_
        max_hr[i] <- NA_real_
      }
    }
    
    cat("\n")
  } else {
    avg_hr <- rep(NA_real_, length(filtered_workout_nodes))
    max_hr <- rep(NA_real_, length(filtered_workout_nodes))
  }
  
} else {
  cat("No heart rate records found in export\n")
  avg_hr <- rep(NA_real_, length(filtered_workout_nodes))
  max_hr <- rep(NA_real_, length(filtered_workout_nodes))
}

# Add heart rate data
running_workouts$avg_heart_rate <- avg_hr
running_workouts$max_heart_rate <- max_hr

cat(paste("Heart rate data found for", sum(!is.na(avg_hr)), "workouts\n"))

# Process dates and calculate metrics
cat("Processing dates and calculating metrics...\n")
running_workouts <- running_workouts %>%
  mutate(
    start_date = ymd_hms(start_date, quiet = TRUE),
    end_date = ymd_hms(end_date, quiet = TRUE),
    creation_date = ymd_hms(creation_date, quiet = TRUE),
    # Calculate pace (min per distance unit)
    pace = if_else(distance > 0, duration / distance, NA_real_),
    # Calculate speed (distance per hour)
    speed = if_else(duration > 0, distance / (duration / 60), NA_real_),
    # Add year, month for easier grouping
    year = year(start_date),
    month = month(start_date),
    week = week(start_date),
    date = as.Date(start_date)
  ) %>%
  arrange(start_date)

# Print summary statistics
cat("\n=== SUMMARY ===\n")
cat(paste("Total running workouts:", nrow(running_workouts), "\n"))
cat(paste("Date range:", min(running_workouts$start_date, na.rm = TRUE), 
          "to", max(running_workouts$start_date, na.rm = TRUE), "\n\n"))

cat("Workouts by source:\n")
print(table(running_workouts$source_name))

cat("\n")
cat(paste("Total distance:", round(sum(running_workouts$distance, na.rm = TRUE), 2), 
          unique(na.omit(running_workouts$distance_unit))[1], "\n"))
cat(paste("Total duration:", round(sum(running_workouts$duration, na.rm = TRUE) / 60, 2), 
          "hours\n"))
cat(paste("Average distance per run:", 
          round(mean(running_workouts$distance, na.rm = TRUE), 2), 
          unique(na.omit(running_workouts$distance_unit))[1], "\n"))
cat(paste("Average pace:", 
          round(mean(running_workouts$pace, na.rm = TRUE), 2), 
          "min per", unique(na.omit(running_workouts$distance_unit))[1], "\n"))

# Identify gaps in training
cat("\n=== GAPS ANALYSIS ===\n")
running_workouts <- running_workouts %>%
  mutate(days_since_last = as.numeric(difftime(start_date, lag(start_date), units = "days")))

gaps <- running_workouts %>%
  filter(days_since_last > 14) %>%
  select(start_date, days_since_last)

if (nrow(gaps) > 0) {
  cat("Gaps longer than 14 days:\n")
  print(head(gaps, 10))
} else {
  cat("No gaps longer than 14 days found.\n")
}

# View first few rows
cat("\n=== SAMPLE DATA ===\n")
print(head(running_workouts %>% 
             select(start_date, distance, duration, pace, avg_heart_rate, source_name), 10))

# Save to CSV
cat("\nSaving to CSV...\n")
write.csv(running_workouts, "running_workouts_w_heart.csv", row.names = FALSE)
cat("Done! Data saved to running_workouts.csv\n")