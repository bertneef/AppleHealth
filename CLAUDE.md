# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains R scripts for parsing and analyzing Apple Health export data, specifically focused on extracting and analyzing running workout data from Apple Health XML exports. The scripts process large XML files to extract workout metadata, heart rate data, and calculate running metrics like pace, speed, and training gaps.

**Active Script**: `Parse2.R` is the current, optimized version of the parser.

## Recent Fix: Heart Rate Data Extraction (2025-10-17)

**Issue Resolved**: Heart rate data is now successfully extracted from WorkoutStatistics elements.

**Root Cause**: The original script attempted to extract HR from individual `<Record>` elements and match them to workouts by timestamp (complex and slow). However, Apple Health exports include pre-calculated HR statistics within `<WorkoutStatistics>` elements nested in each `<Workout>` node.

**Solution**: Modified Parse2.R to extract HR directly from WorkoutStatistics elements alongside distance and energy data. This approach is simpler, faster, and more reliable. The script now extracts:
- `avg_heart_rate` (average HR during workout)
- `max_heart_rate` (maximum HR during workout)
- `min_heart_rate` (minimum HR during workout - new field)

**Data Coverage**:
- Apple Watch workouts (watchOS 9.0+): ~4,101 workouts have WorkoutStatistics with pre-calculated HR data
- Nike Run Club workouts: HR data stored only as individual Record elements, requires timestamp matching to extract
- Earlier Apple Watch workouts: May not have embedded HR data in WorkoutStatistics

**Note**: Nike Run Club and some older Apple Watch workouts require a fallback to the timestamp-matching approach to extract heart rate data from individual Record elements.

## Running the Scripts

### Execute the main parsing script
```bash
Rscript parse.R
```

### Execute the optimized parsing script (recommended for large files)
```bash
Rscript Parse2.R
```

## Architecture

### Data Pipeline

The scripts follow a multi-stage pipeline architecture:

1. **XML Loading**: Read the Apple Health export XML file (typically very large, >100MB)
2. **Workout Extraction**: Use XPath queries to extract all `<Workout>` nodes
3. **Filtering**: Filter for:
   - Running workouts (`HKWorkoutActivityTypeRunning`)
   - Specific sources (Apple Watch/Workout app/Nike Run Club)
4. **Attribute Extraction**: Extract workout metadata (dates, duration, device info)
5. **Statistics Extraction**: Parse nested `<WorkoutStatistics>` elements for:
   - Distance (`HKQuantityTypeIdentifierDistanceWalkingRunning`)
   - Energy (`HKQuantityTypeIdentifierActiveEnergyBurned`)
6. **Heart Rate Processing**: Extract and match heart rate records to workouts:
   - Pre-filter HR records to workout time range (optimization)
   - Match HR records to individual workouts by timestamp
   - Calculate average and maximum heart rate per workout
7. **Metrics Calculation**: Compute pace, speed, and temporal features
8. **Gap Analysis**: Identify training gaps >14 days
9. **Output**: Export to CSV for further analysis

### Key Optimization Strategies

The scripts implement several performance optimizations for handling large XML files:

- **Vectorized Operations**: Use vectorized extraction with `xml_attr()` instead of loops where possible
- **Early Filtering**: Filter workouts by source before extracting detailed data
- **Time Range Filtering**: Only extract heart rate records within the overall workout time range
- **Numeric Timestamp Comparison**: Convert dates to numeric for faster matching
- **Chunked Processing**: Process large operations in chunks to manage memory (Parse2.R)
- **Memory Management**: Explicitly remove large objects and call `gc()` (Parse2.R)

### Script Differences

- **parse.R**: Original implementation with verbose progress logging (legacy)
- **Parse2.R**: **Current active version** - Optimized implementation with:
  - Combined filtering (running + source filter in one pass)
  - Chunked statistics extraction
  - More aggressive memory management
  - Better suited for very large export files (>500MB)

## Data Structure

### Input
- Apple Health export XML file (`export.xml`) from iOS Health app
- Location must be updated in script: see line 6-7 of either script

### Output
- Excel file: `running_workouts.xlsx` (Parse2.R)
- Legacy: CSV files from older versions (parse.R outputs `running_workouts_w_heart.csv`)
- Columns include:
  - Temporal: `start_date`, `end_date`, `creation_date`, `year`, `month`, `week`, `date`
  - Workout: `duration`, `distance`, `energy`
  - Performance: `pace`, `speed`, `avg_heart_rate`, `max_heart_rate`, `min_heart_rate`
  - Metadata: `source_name`, `source_version`, `device`
  - Training: `days_since_last` (gap detection)

## Dependencies

Required R packages:
- `xml2`: XML parsing
- `dplyr`: Data manipulation
- `lubridate`: Date/time handling
- `writexl`: Excel file output

Install with:
```R
install.packages(c("xml2", "dplyr", "lubridate", "writexl"))
```

## Important Notes

- **File Path Configuration**: Update `xml_file` variable (line 6-7) to point to your Apple Health export location
- **Processing Time**: Large exports (multiple years of data) can take 5-15 minutes to process
- **Memory Usage**: Processing requires significant RAM for large files; Parse2.R is more memory-efficient
- **Heart Rate Matching**: HR records are matched to workouts by timestamp overlap; records outside workout windows are excluded
- **Source Filtering**: Only processes workouts from Apple Watch, Workout app, or Nike Run Club (filters out third-party apps)

## Known Limitations & Future Improvements

### Duplicate Workouts
**Issue**: When running with multiple tracking apps simultaneously (e.g., WorkOutDoors for route + Nike Run Club for tracking), both workouts are included in the output, causing double-counting.

**Current Workaround**: Manual filtering of the CSV output.

**Planned Solution**: Implement overlap detection and auto-deduplication with the following logic:
- Detect overlapping workouts (>50% time overlap)
- Priority: Nike Run Club is primary source
- Exception: If Nike workout is suspiciously short (<50% distance) compared to overlapping workout AND the other workout is much longer (>1.5x duration), keep the other workout instead
- Tie-breaker: Keep workout with more complete data (HR, distance, energy)

**Implementation approach**:
1. Add overlap detection flags to CSV for manual review (`has_overlap`, `overlaps_with_idx`)
2. Later: Implement auto-deduplication with configurable rules
3. Insert after date processing (Parse2.R:132) and before metrics calculation (Parse2.R:145)
