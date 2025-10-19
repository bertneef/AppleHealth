library(xml2)
library(dplyr)
library(lubridate)
library(writexl)

# Read the Apple Health export XML file
xml_file <- "/Users/bertneef/Library/Mobile Documents/com~apple~CloudDocs/Bert/Running/apple_health_export/export.xml"

cat("Reading XML file (this will take a while for large files)...\n")
health_data <- read_xml(xml_file)

# OPTIMIZATION: Use XPath to extract only what we need, avoiding full tree traversal
cat("Extracting running workouts with XPath...\n")

# Extract all workouts at once (faster than filtering after)
all_workouts <- xml_find_all(health_data, "//Workout")
cat(paste("Found", length(all_workouts), "total workouts\n"))

# Vectorized extraction - get all attributes at once
cat("Extracting workout attributes...\n")
workout_types <- xml_attr(all_workouts, "workoutActivityType")
source_names <- xml_attr(all_workouts, "sourceName")

# Combined filter: running + Apple/Nike sources (do it once, not twice)
cat("Filtering for running workouts from Apple/Nike...\n")
combined_filter <- (workout_types == "HKWorkoutActivityTypeRunning") & 
  grepl("Watch|Workout|Nike", source_names, ignore.case = TRUE)

workouts <- all_workouts[combined_filter]
cat(paste("Filtered to", length(workouts), "running workouts from Apple/Nike\n"))

if (length(workouts) == 0) {
  stop("No running workouts found from Apple Watch or Nike Run Club")
}

# Keep all data in memory (system has plenty of RAM)

# Extract basic attributes (vectorized)
cat("Extracting workout data...\n")
running_workouts <- data.frame(
  source_name = source_names[combined_filter],
  source_version = xml_attr(workouts, "sourceVersion"),
  device = xml_attr(workouts, "device"),
  creation_date = xml_attr(workouts, "creationDate"),
  start_date = xml_attr(workouts, "startDate"),
  end_date = xml_attr(workouts, "endDate"),
  duration = as.numeric(xml_attr(workouts, "duration")),
  duration_unit = xml_attr(workouts, "durationUnit"),
  stringsAsFactors = FALSE
)

# Keep all data in memory

# Extract distance, energy, and heart rate from WorkoutStatistics
cat("Extracting workout statistics (distance, energy, heart rate)...\n")

# Process in chunks to manage memory
chunk_size <- 500
n_workouts <- length(workouts)
n_chunks <- ceiling(n_workouts / chunk_size)

distance_vals <- numeric(n_workouts)
distance_units <- character(n_workouts)
energy_vals <- numeric(n_workouts)
energy_units <- character(n_workouts)
avg_hr_vals <- numeric(n_workouts)
max_hr_vals <- numeric(n_workouts)
min_hr_vals <- numeric(n_workouts)

# HR zone tracking (max HR = 180)
zone1_mins <- numeric(n_workouts)  # 50-60% = 90-108 bpm
zone2_mins <- numeric(n_workouts)  # 60-70% = 108-126 bpm
zone3_mins <- numeric(n_workouts)  # 70-80% = 126-144 bpm
zone4_mins <- numeric(n_workouts)  # 80-90% = 144-162 bpm
zone5_mins <- numeric(n_workouts)  # 90-100% = 162-180 bpm

# Indoor workout tracking
is_indoor <- logical(n_workouts)

for (chunk in 1:n_chunks) {
  if (n_chunks > 1) cat(paste0("Processing chunk ", chunk, "/", n_chunks, "\r"))

  start_idx <- (chunk - 1) * chunk_size + 1
  end_idx <- min(chunk * chunk_size, n_workouts)

  for (i in start_idx:end_idx) {
    w <- workouts[[i]]

    # Distance
    dist_node <- xml_find_first(w, './/WorkoutStatistics[@type="HKQuantityTypeIdentifierDistanceWalkingRunning"]')
    if (length(dist_node) > 0 && !is.na(dist_node)) {
      distance_vals[i] <- as.numeric(xml_attr(dist_node, "sum"))
      distance_units[i] <- xml_attr(dist_node, "unit")
    } else {
      distance_vals[i] <- NA_real_
      distance_units[i] <- NA_character_
    }

    # Energy
    energy_node <- xml_find_first(w, './/WorkoutStatistics[@type="HKQuantityTypeIdentifierActiveEnergyBurned"]')
    if (length(energy_node) > 0 && !is.na(energy_node)) {
      energy_vals[i] <- as.numeric(xml_attr(energy_node, "sum"))
      energy_units[i] <- xml_attr(energy_node, "unit")
    } else {
      energy_vals[i] <- NA_real_
      energy_units[i] <- NA_character_
    }

    # Heart Rate (embedded in WorkoutStatistics)
    hr_node <- xml_find_first(w, './/WorkoutStatistics[@type="HKQuantityTypeIdentifierHeartRate"]')
    if (length(hr_node) > 0 && !is.na(hr_node)) {
      avg_hr_vals[i] <- as.numeric(xml_attr(hr_node, "average"))
      max_hr_vals[i] <- as.numeric(xml_attr(hr_node, "maximum"))
      min_hr_vals[i] <- as.numeric(xml_attr(hr_node, "minimum"))
    } else {
      avg_hr_vals[i] <- NA_real_
      max_hr_vals[i] <- NA_real_
      min_hr_vals[i] <- NA_real_
    }

    # Indoor workout detection
    indoor_node <- xml_find_first(w, './/MetadataEntry[@key="HKIndoorWorkout"]')
    if (length(indoor_node) > 0 && !is.na(indoor_node)) {
      indoor_val <- xml_attr(indoor_node, "value")
      is_indoor[i] <- (indoor_val == "1")
    } else {
      is_indoor[i] <- FALSE  # Default to outdoor if not specified
    }
  }
}

cat("\n")

running_workouts$distance <- distance_vals
running_workouts$distance_unit <- distance_units
running_workouts$energy <- energy_vals
running_workouts$energy_unit <- energy_units
running_workouts$avg_heart_rate <- avg_hr_vals
running_workouts$max_heart_rate <- max_hr_vals
running_workouts$min_heart_rate <- min_hr_vals
running_workouts$zone1_minutes <- zone1_mins
running_workouts$zone2_minutes <- zone2_mins
running_workouts$zone3_minutes <- zone3_mins
running_workouts$zone4_minutes <- zone4_mins
running_workouts$zone5_minutes <- zone5_mins
running_workouts$is_indoor <- is_indoor

# Convert dates once
cat("Processing dates...\n")
running_workouts <- running_workouts %>%
  mutate(
    start_date = ymd_hms(start_date, quiet = TRUE),
    end_date = ymd_hms(end_date, quiet = TRUE),
    creation_date = ymd_hms(creation_date, quiet = TRUE)
  )

cat(paste("\nWorkout time range:",
          min(running_workouts$start_date, na.rm = TRUE),
          "to",
          max(running_workouts$end_date, na.rm = TRUE), "\n"))

cat(paste("Heart rate data found for",
          sum(!is.na(running_workouts$avg_heart_rate)),
          "out of",
          nrow(running_workouts),
          "workouts (from WorkoutStatistics)\n"))

# OVERLAP DETECTION
cat("\nDetecting overlapping workouts...\n")

# Initialize overlap columns
running_workouts$has_overlap <- FALSE
running_workouts$overlaps_with_idx <- NA_character_
running_workouts$keep_workout <- TRUE
running_workouts$remove_reason <- NA_character_

# Sort by start date for efficient comparison
running_workouts <- running_workouts %>% arrange(start_date)

# Check each workout against others
n <- nrow(running_workouts)
for (i in 1:(n-1)) {
  # Only check workouts that haven't been marked as overlapping yet
  if (running_workouts$has_overlap[i]) next

  w1_start <- running_workouts$start_date[i]
  w1_end <- running_workouts$end_date[i]
  w1_duration <- running_workouts$duration[i]
  w1_distance <- running_workouts$distance[i]
  w1_source <- running_workouts$source_name[i]

  # Look for overlaps within a reasonable time window (same day)
  for (j in (i+1):n) {
    w2_start <- running_workouts$start_date[j]

    # Stop if we're beyond possible overlap (more than 24 hours later)
    if (as.numeric(difftime(w2_start, w1_start, units = "hours")) > 24) break

    w2_end <- running_workouts$end_date[j]
    w2_duration <- running_workouts$duration[j]
    w2_distance <- running_workouts$distance[j]
    w2_source <- running_workouts$source_name[j]

    # Calculate overlap
    overlap_start <- max(w1_start, w2_start)
    overlap_end <- min(w1_end, w2_end)

    if (overlap_start < overlap_end) {
      # There is time overlap
      overlap_duration <- as.numeric(difftime(overlap_end, overlap_start, units = "secs"))
      overlap_pct_w1 <- overlap_duration / w1_duration
      overlap_pct_w2 <- overlap_duration / w2_duration

      # Significant overlap if >50% of either workout overlaps
      if (overlap_pct_w1 > 0.5 || overlap_pct_w2 > 0.5) {
        # Mark both as having overlap
        running_workouts$has_overlap[i] <- TRUE
        running_workouts$has_overlap[j] <- TRUE

        # Track which workouts overlap (store as comma-separated list of indices)
        if (is.na(running_workouts$overlaps_with_idx[i])) {
          running_workouts$overlaps_with_idx[i] <- as.character(j)
        } else {
          running_workouts$overlaps_with_idx[i] <- paste(running_workouts$overlaps_with_idx[i], j, sep = ",")
        }

        if (is.na(running_workouts$overlaps_with_idx[j])) {
          running_workouts$overlaps_with_idx[j] <- as.character(i)
        } else {
          running_workouts$overlaps_with_idx[j] <- paste(running_workouts$overlaps_with_idx[j], i, sep = ",")
        }

        # DECISION LOGIC: Which workout to keep?
        # Priority 1: Nike Run Club is primary source (unless suspiciously short)
        is_nike_1 <- grepl("Nike", w1_source, ignore.case = TRUE)
        is_nike_2 <- grepl("Nike", w2_source, ignore.case = TRUE)

        # Check if Nike workout is suspiciously short
        if (is_nike_1 && !is_nike_2) {
          # Nike is workout 1
          if (!is.na(w1_distance) && !is.na(w2_distance) &&
              w1_distance < 0.5 * w2_distance && w2_duration > 1.5 * w1_duration) {
            # Nike workout is suspiciously short, keep the other one
            running_workouts$keep_workout[i] <- FALSE
            running_workouts$remove_reason[i] <- "Nike workout suspiciously short compared to overlapping workout"
          } else {
            # Keep Nike, remove the other
            running_workouts$keep_workout[j] <- FALSE
            running_workouts$remove_reason[j] <- "Duplicate: Nike Run Club is primary source"
          }
        } else if (is_nike_2 && !is_nike_1) {
          # Nike is workout 2
          if (!is.na(w2_distance) && !is.na(w1_distance) &&
              w2_distance < 0.5 * w1_distance && w1_duration > 1.5 * w2_duration) {
            # Nike workout is suspiciously short, keep the other one
            running_workouts$keep_workout[j] <- FALSE
            running_workouts$remove_reason[j] <- "Nike workout suspiciously short compared to overlapping workout"
          } else {
            # Keep Nike, remove the other
            running_workouts$keep_workout[i] <- FALSE
            running_workouts$remove_reason[i] <- "Duplicate: Nike Run Club is primary source"
          }
        } else {
          # Neither is Nike, or both are Nike - use tie-breaker
          # Priority 2: Keep workout with more complete data
          w1_completeness <- sum(!is.na(c(w1_distance, running_workouts$avg_heart_rate[i],
                                          running_workouts$energy[i])))
          w2_completeness <- sum(!is.na(c(w2_distance, running_workouts$avg_heart_rate[j],
                                          running_workouts$energy[j])))

          if (w1_completeness > w2_completeness) {
            running_workouts$keep_workout[j] <- FALSE
            running_workouts$remove_reason[j] <- "Duplicate: Other workout has more complete data"
          } else if (w2_completeness > w1_completeness) {
            running_workouts$keep_workout[i] <- FALSE
            running_workouts$remove_reason[i] <- "Duplicate: Other workout has more complete data"
          } else {
            # Same completeness - keep the longer workout
            if (w1_duration >= w2_duration) {
              running_workouts$keep_workout[j] <- FALSE
              running_workouts$remove_reason[j] <- "Duplicate: Other workout is longer"
            } else {
              running_workouts$keep_workout[i] <- FALSE
              running_workouts$remove_reason[i] <- "Duplicate: Other workout is longer"
            }
          }
        }
      }
    }
  }
}

overlap_count <- sum(running_workouts$has_overlap)
remove_count <- sum(!running_workouts$keep_workout)
cat(paste("Found", overlap_count, "workouts with overlaps\n"))
cat(paste("Marked", remove_count, "workouts for removal\n"))

# FALLBACK: Extract HR from individual Records for workouts missing HR data
# (e.g., Nike Run Club, older Apple Watch workouts)
workouts_missing_hr <- which(is.na(running_workouts$avg_heart_rate))

if (length(workouts_missing_hr) > 0) {
  cat(paste("\nExtracting HR from individual Records for",
            length(workouts_missing_hr),
            "workouts without WorkoutStatistics...\n"))

  # Get time range for workouts missing HR
  min_time <- min(running_workouts$start_date[workouts_missing_hr], na.rm = TRUE)
  max_time <- max(running_workouts$end_date[workouts_missing_hr], na.rm = TRUE)

  cat("Fetching HR records...\n")

  # XPath with attribute filter doesn't work, so we find all Records first then filter
  all_records <- xml_find_all(health_data, '//Record')
  cat(paste("Total Record elements found:", length(all_records), "\n"))

  # Filter by type attribute (vectorized)
  record_types <- xml_attr(all_records, "type")
  hr_filter <- record_types == "HKQuantityTypeIdentifierHeartRate"
  hr_records <- all_records[hr_filter]
  cat(paste("HR records found:", length(hr_records), "\n"))

  if (length(hr_records) > 0) {
    cat("Processing HR records...\n")
    cat(paste("Found", length(hr_records), "total HR records\n"))

    # Extract and filter HR data
    cat("Filtering HR records to workout time range...\n")
    hr_starts_raw <- xml_attr(hr_records, "startDate")
    hr_values_raw <- xml_attr(hr_records, "value")

    hr_starts <- ymd_hms(hr_starts_raw, quiet = TRUE)
    hr_values <- as.numeric(hr_values_raw)

    # Filter to time range of workouts missing HR
    time_filter <- !is.na(hr_starts) & !is.na(hr_values) &
      hr_starts >= min_time & hr_starts <= max_time

    hr_data <- data.frame(
      start = hr_starts[time_filter],
      value = hr_values[time_filter],
      stringsAsFactors = FALSE
    )

    cat(paste("Filtered to", nrow(hr_data), "HR records in time range\n"))

    if (nrow(hr_data) > 0) {
      hr_data <- hr_data[order(hr_data$start), ]
      hr_timestamps <- as.numeric(hr_data$start)

      cat("Matching HR records to workouts...\n")

      for (i in workouts_missing_hr) {
        if (i %% 20 == 0) cat(paste0("  Processing workout ", which(workouts_missing_hr == i), "/", length(workouts_missing_hr), "\r"))

        workout_start <- as.numeric(running_workouts$start_date[i])
        workout_end <- as.numeric(running_workouts$end_date[i])

        matches <- hr_timestamps >= workout_start & hr_timestamps <= workout_end

        if (any(matches)) {
          matching_vals <- hr_data$value[matches]
          matching_times <- hr_data$start[matches]

          running_workouts$avg_heart_rate[i] <- mean(matching_vals)
          running_workouts$max_heart_rate[i] <- max(matching_vals)
          running_workouts$min_heart_rate[i] <- min(matching_vals)

          # Calculate time in each HR zone
          # Assume uniform time between readings
          n_readings <- length(matching_vals)
          if (n_readings > 1) {
            # Calculate average interval between readings in minutes
            time_span_sec <- as.numeric(difftime(max(matching_times), min(matching_times), units = "secs"))
            avg_interval_min <- time_span_sec / (n_readings - 1) / 60

            # Count readings in each zone and multiply by interval
            zone1_mins[i] <- sum(matching_vals >= 90 & matching_vals < 108) * avg_interval_min
            zone2_mins[i] <- sum(matching_vals >= 108 & matching_vals < 126) * avg_interval_min
            zone3_mins[i] <- sum(matching_vals >= 126 & matching_vals < 144) * avg_interval_min
            zone4_mins[i] <- sum(matching_vals >= 144 & matching_vals < 162) * avg_interval_min
            zone5_mins[i] <- sum(matching_vals >= 162) * avg_interval_min
          }
        }
      }

      cat("\n")
    }
  }

  cat(paste("Total workouts with HR data:",
            sum(!is.na(running_workouts$avg_heart_rate)),
            "out of",
            nrow(running_workouts), "\n"))

  # Update zone columns after fallback extraction
  running_workouts$zone1_minutes <- zone1_mins
  running_workouts$zone2_minutes <- zone2_mins
  running_workouts$zone3_minutes <- zone3_mins
  running_workouts$zone4_minutes <- zone4_mins
  running_workouts$zone5_minutes <- zone5_mins
}

# Calculate metrics
cat("\nCalculating pace, speed, and HR zone percentages...\n")
running_workouts <- running_workouts %>%
  mutate(
    pace = if_else(distance > 0, duration / distance, NA_real_),
    speed = if_else(duration > 0, distance / (duration / 60), NA_real_),
    year = year(start_date),
    month = month(start_date),
    week = week(start_date),
    date = as.Date(start_date),
    # Calculate zone percentages
    total_zone_time = zone1_minutes + zone2_minutes + zone3_minutes + zone4_minutes + zone5_minutes,
    zone1_percent = if_else(total_zone_time > 0, zone1_minutes / total_zone_time * 100, NA_real_),
    zone2_percent = if_else(total_zone_time > 0, zone2_minutes / total_zone_time * 100, NA_real_),
    zone3_percent = if_else(total_zone_time > 0, zone3_minutes / total_zone_time * 100, NA_real_),
    zone4_percent = if_else(total_zone_time > 0, zone4_minutes / total_zone_time * 100, NA_real_),
    zone5_percent = if_else(total_zone_time > 0, zone5_minutes / total_zone_time * 100, NA_real_)
  ) %>%
  arrange(start_date)

# Print summary
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

# Gaps analysis
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

# Sample data
cat("\n=== SAMPLE DATA ===\n")
print(head(running_workouts %>% 
             select(start_date, distance, duration, pace, avg_heart_rate, source_name), 10))

# Save to Excel
cat("\nSaving to Excel...\n")
write_xlsx(running_workouts, "running_workouts.xlsx")
cat("Done! Data saved to running_workouts.xlsx\n")