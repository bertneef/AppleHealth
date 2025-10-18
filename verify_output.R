library(openxlsx)

data <- read.xlsx("running_workouts.xlsx")

cat("=== VERIFICATION REPORT ===\n\n")

cat("1. Indoor workout detection:\n")
cat(paste("  Indoor workouts:", sum(data$is_indoor, na.rm=TRUE), "out of", nrow(data), "\n"))
cat(paste("  Outdoor workouts:", sum(!data$is_indoor, na.rm=TRUE), "\n\n"))

cat("2. Heart rate zone data:\n")
zone_workouts <- sum(rowSums(data[, grep("zone.*minutes", names(data))], na.rm=TRUE) > 0)
cat(paste("  Workouts with zone data:", zone_workouts, "out of", nrow(data), "\n"))
cat(paste("  Zone 1 avg:", round(mean(data$zone1_minutes[data$zone1_minutes > 0], na.rm=TRUE), 1), "min\n"))
cat(paste("  Zone 2 avg:", round(mean(data$zone2_minutes[data$zone2_minutes > 0], na.rm=TRUE), 1), "min\n"))
cat(paste("  Zone 3 avg:", round(mean(data$zone3_minutes[data$zone3_minutes > 0], na.rm=TRUE), 1), "min\n"))
cat(paste("  Zone 4 avg:", round(mean(data$zone4_minutes[data$zone4_minutes > 0], na.rm=TRUE), 1), "min\n"))
cat(paste("  Zone 5 avg:", round(mean(data$zone5_minutes[data$zone5_minutes > 0], na.rm=TRUE), 1), "min\n\n"))

cat("3. Heart rate data coverage:\n")
cat(paste("  Workouts with HR data:", sum(!is.na(data$avg_heart_rate)), "out of", nrow(data), "\n\n"))

cat("4. Sample workouts with all features:\n")
sample_idx <- which(data$zone1_minutes > 0 | data$zone2_minutes > 0 | data$zone3_minutes > 0)
if (length(sample_idx) > 0) {
  sample <- head(data[sample_idx, c('start_date', 'source_name', 'is_indoor', 'avg_heart_rate',
                                     'zone1_minutes', 'zone2_minutes', 'zone3_minutes',
                                     'zone4_minutes', 'zone5_minutes')], 5)
  print(sample)
} else {
  cat("  No workouts with zone data found\n")
}

cat("\n5. Indoor vs Outdoor comparison:\n")
indoor_data <- data[data$is_indoor == TRUE & !is.na(data$avg_heart_rate), ]
outdoor_data <- data[data$is_indoor == FALSE & !is.na(data$avg_heart_rate), ]

if (nrow(indoor_data) > 0) {
  cat(paste("  Indoor avg HR:", round(mean(indoor_data$avg_heart_rate, na.rm=TRUE), 1), "bpm\n"))
  cat(paste("  Indoor avg pace:", round(mean(indoor_data$pace, na.rm=TRUE), 2), "min/km\n"))
}

if (nrow(outdoor_data) > 0) {
  cat(paste("  Outdoor avg HR:", round(mean(outdoor_data$avg_heart_rate, na.rm=TRUE), 1), "bpm\n"))
  cat(paste("  Outdoor avg pace:", round(mean(outdoor_data$pace, na.rm=TRUE), 2), "min/km\n"))
}
