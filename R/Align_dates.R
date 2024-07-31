#' @title Infer BLE LTER field campaign season from date or date-times
#' @description Infer BLE LTER field campaign season from date or date-times: dates in May or earlier are under ice, June to 15th of July is break up, and 16th of July or later is open water. Do not use with mooring data.
#' @param data (data.frame) A data.frame with date
#' @param date_col (character) Name of column with dates or date-times. Please convert this column into a Date format first (not just strings containing dates).
#' @return A vector of strings: field campaign seasons in same order as dates or date-times supplied.
#' @export
infer_season_new <- function(date_time) {
  months <- month(date_time)
  days <- mday(date_time)
  seasons <- ifelse(
    months <= 5,
    "under ice",
    ifelse(
      months == 6,
      "break up",
      ifelse(
        months >= 8,
        "open water",
        ifelse(days > 15, "open water", "break up")
      )
    )
  )
  
  return(seasons)
}
#' Check Data Matches Function
#' This function compares user-provided input data with an official record of station data.
#' It verifies alignment of date_time, station, year, and season data between the two datasets.
#' When mismatches are found, it attempts to correct them where possible and reports the results.
#' @param input_data A data frame containing the user-provided data with columns: `station`, `date_time`, `year`, and `season`.
#' @param official_record A data frame containing the official record data with columns: `station`, `date_time`, `year`, and `season`.
#' @return A data frame with columns:
#' - `station`: The station identifier.
#' - `season`: The season identifier.
#' - `original_date_time`: The original date and time from the input data.
#' - `date_time_changed`: The date and time after attempting corrections.
#' - `expected_date_time`: The expected date and time from the official record.
#' - `year`: The year identifier.
#' - `match_status`: A status message describing the match results.
#' - `result`: A boolean indicating whether the final date and time matches the expected date and time.
check_data_matches <- function(input_data, official_record) {
  check_matches <- function(row) {
    station_matches <- official_record[official_record$station == row$station, ]
    result_list <- list(original_date_time = row$date_time, date_time_changed = row$date_time,
                        expected_date_time = NA, match_status = "No station match", result = FALSE)
    
    if (nrow(station_matches) > 0) {
      year_matches <- station_matches[station_matches$year == row$year, ]
      
      if (nrow(year_matches) > 0) {
        season_matches <- year_matches[year_matches$season == row$season, ]
        
        if (nrow(season_matches) > 0) {
          correct_dates <- season_matches$date_time
          
          if (length(correct_dates) > 1) {
            if (row$date_time %in% correct_dates) {
              result_list$match_status <- "Full match found"
              result_list$expected_date_time <- row$date_time
              result_list$date_time_changed <- row$date_time
            } else {
              result_list$match_status <- paste("Multiple dates found in CP data, none match:", toString(correct_dates))
              result_list$expected_date_time <- NA
              result_list$result <- FALSE
            }
          } else {
            result_list$expected_date_time <- correct_dates
            if (row$date_time != correct_dates) {
              result_list$date_time_changed <- correct_dates  # Update if mismatch
              result_list$match_status <- paste("Date/time updated")
            } else {
              result_list$match_status <- "Full match found"
            }
          }
          result_list$result <- result_list$date_time_changed == result_list$expected_date_time
        } else {
          result_list$match_status <- "Year match but no season match"
        }
      } else {
        result_list$match_status <- "Station match but no year match"
      }
    }
    
    return(result_list)
  }
  
  results <- rowwise(input_data) %>%
    mutate(match_details = list(check_matches(cur_data())),
           original_date_time = match_details$original_date_time,
           date_time_changed = match_details$date_time_changed,
           expected_date_time = match_details$expected_date_time,
           match_status = match_details$match_status,
           result = match_details$result) %>%
    ungroup() %>%
    select(station, season, original_date_time, date_time_changed, expected_date_time, year, match_status, result)
  
  return(results)
}
#' Align Dates Function
#' This function aligns the date and time data in the user-provided data frame with an official record.
#' It reads the official record from a given CSV URL, infers the year and season for both datasets, checks for matches, updates mismatches where possible, and writes the results to an Excel file.
#' @param df A data frame containing the user-provided data with a column: `date_time`.
#' @details The function performs the following steps:
#' - Reads the official record from a provided CSV URL.
#' - Infers the `year` and `season` from the `date_time` column for both the input data and the official record.
#' - Calls the `check_data_matches` function to compare and align the date and time data.
#' - Writes the results to an Excel file at a specified file path.
#' @return None. The function writes the output to an Excel file and prints a confirmation message.
Align_dates <- function(df) {
  official_record <- read.csv("https://utexas.box.com/shared/static/9hcctqqilisc0t61wbbdiziig8ok8rg8.csv")
  
  df$year <- year(ymd_hms(df$date_time))
  df$season <- infer_season_new(df$date_time)
  official_record$season <- infer_season_new(ymd_hms(official_record$date_time))
  official_record$year <- year(ymd_hms(official_record$date_time))
  
  results_data <- check_data_matches(df, official_record)
  output_file_path <- "C:/Users/im23237/Documents/data30.xlsx"
  write.xlsx(results_data, output_file_path, rowNames = FALSE)
  print(paste("Results have been written to", output_file_path))
}