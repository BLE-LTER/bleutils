#' Check Data Matches Function
#' This function compares user-provided input data with an official record of station data.
#' It verifies alignment of date_time, station, year, and season data between the two datasets.
#' When mismatches are found, it attempts to correct them where possible and reports the results.
#' @param input_data A data frame containing the user-provided data with columns: `station`, `date_time`, `year_date_time`, and `season_date_time`.
#' @param official_record A data frame containing the official record data with columns: `station`, `date_time`, `year_date_time`, and `season_date_time`.
#' @return A data frame with columns:
#' - `station`: The station identifier.
#' - `season_date_time`: The season identifier.
#' - `original_date_time`: The original date and time from the input data.
#' - `date_time_changed`: The date and time after attempting corrections.
#' - `expected_date_time`: The expected date and time from the official record.
#' - `year_date_time`: The year identifier.
#' - `match_status`: A status message describing the match results.
#' - `result`: A boolean indicating whether the final date and time matches the expected date and time.
check_data_matches <- function(input_data, official_record) {
  check_matches <- function(row) {
    station_matches <- official_record[official_record$station == row$station, ]
    result_list <- list(original_date_time = row$date_time, date_time_changed = row$date_time,
                        expected_date_time = NA, match_status = "No station match", result = FALSE)
    
    if (nrow(station_matches) > 0) {
      year_matches <- station_matches[station_matches$year_date_time == row$year_date_time, ]
      
      if (nrow(year_matches) > 0) {
        season_matches <- year_matches[year_matches$season_date_time == row$season_date_time, ]
        
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
    select(station, season_date_time, original_date_time, date_time_changed, expected_date_time, year_date_time, match_status, result)
  
  return(results)
}
#' Align_dates
#' This function is designed to align and verify date and time data between a given dataframe and an official record stored in a CSV file. 
#' It infers the seasonal categorization of dates in the dataset, checks for mismatches between the dataset and the official record, and saves the results to an Excel file specified by the user.
#' @param df A dataframe containing the columns `date_time` (with datetime values). The function will add two new columns to this dataframe: `year_date_time` (the year extracted from `date_time`) and `season_date_time` (the inferred season based on `date_time`).
#' @return The function does not return a value. It performs the following steps:
#'   1. Infers the season from the `date_time` column in both the input dataframe and the official record.
#'   2. Checks for mismatches between the input dataframe and the official record using the `check_data_matches` function.
#'   3. Prompts the user to specify an output file path for saving the results.
#'   4. Writes the results to an Excel file at the specified location.
#' @details
#' The function `infer_season_date_time` is an internal helper function used to categorize each datetime into a season:
#' - "under ice" for dates in January through May
#' - "break up" for dates in June
#' - "open water" for dates in August and later, or if the date is after the 15th of July
#' The official record is fetched from a remote CSV file, and seasonal and yearly categorizations are applied similarly.
#' The function then uses the `check_data_matches` function (not provided in this script) to compare the input dataframe with the official record.
#' Finally, the user is prompted to provide an output file path, and the results are saved as an Excel file.
Align_dates <- function(df) {
  
  infer_season_date_time <- function(date_time) {
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
  
  official_record <- read.csv("https://utexas.box.com/shared/static/9hcctqqilisc0t61wbbdiziig8ok8rg8.csv")
  
  df$year_date_time <- year(ymd_hms(df$date_time))
  df$season_date_time <- infer_season_date_time(df$date_time)
  official_record$season_date_time <- infer_season_date_time(ymd_hms(official_record$date_time))
  official_record$year_date_time <- year(ymd_hms(official_record$date_time))
  
  results_data <- check_data_matches(df, official_record)
  
  # Prompt the user for the output file path
  output_file_path <- readline(prompt = "Please enter the output file path (including file name): ")
  
  write.xlsx(results_data, output_file_path, rowNames = FALSE)
  print(paste("Results have been written to", output_file_path))
}