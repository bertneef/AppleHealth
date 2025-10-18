library(openxlsx)

# Read the Excel file back in
data <- read.xlsx("running_workouts.xlsx")

cat("Column names:\n")
print(names(data))

cat("\n\nZone columns present:\n")
zone_cols <- grep("zone", names(data), ignore.case = TRUE, value = TRUE)
print(zone_cols)

cat("\n\nZone data summary:\n")
if (length(zone_cols) > 0) {
  print(summary(data[, zone_cols]))

  cat("\n\nRows with non-zero zone data:\n")
  has_zone_data <- rowSums(data[, grep("zone.*minutes", names(data))], na.rm = TRUE) > 0
  cat(paste("Workouts with zone data:", sum(has_zone_data), "out of", nrow(data), "\n"))

  if (sum(has_zone_data) > 0) {
    cat("\nSample workouts with zone data:\n")
    sample_data <- data[has_zone_data, c("start_date", "source_name", "zone1_minutes", "zone2_minutes", "zone3_minutes", "zone4_minutes", "zone5_minutes")]
    print(head(sample_data, 10))
  }
} else {
  cat("No zone columns found!\n")
}
