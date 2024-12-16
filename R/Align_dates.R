#' Check Data Matches Function
#' This function compares user-provided input data with an official record of station data.
#' It verifies alignment of date_time, station, year, and season data between the two datasets.
#' When mismatches are found, it attempts to correct them where possible and reports the results.
#' @param input_data A data frame containing the user-provided data with columns: station, date_time, year_date_time, and season_date_time.
#' @param official_record A data frame containing the official record data with columns: station, date_time, year_date_time, and season_date_time.
#' @return A data frame with columns:
#' - station: The station identifier.
#' - season_date_time: The season identifier.
#' - original_date_time: The original date and time from the input data.
#' - date_time_changed: The date and time after attempting corrections.
#' - expected_date_time: The expected date and time from the official record.
#' - year_date_time: The year identifier.
#' - match_status: A status message describing the match results.
#' - result: A boolean indicating whether the final date and time matches the expected date and time.
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
#' @param df A dataframe containing the columns date_time (with datetime values). The function will add two new columns to this dataframe: year_date_time (the year extracted from date_time) and season_date_time (the inferred season based on date_time).
#' @return The function does not return a value. It performs the following steps:
#'   1. Infers the season from the date_time column in both the input dataframe and the official record.
#'   2. Checks for mismatches between the input dataframe and the official record using the check_data_matches function.
#'   3. Prompts the user to specify an output file path for saving the results.
#'   4. Writes the results to an Excel file at the specified location.
#' @details
#' The function infer_season_date_time is an internal helper function used to categorize each datetime into a season:
#' - "under ice" for dates in January through May
#' - "break up" for dates in June
#' - "open water" for dates in August and later, or if the date is after the 15th of July
#' The official record is fetched from a remote CSV file, and seasonal and yearly categorizations are applied similarly.
#' The function then uses the check_data_matches function (not provided in this script) to compare the input dataframe with the official record.
#' Finally, the user is prompted to provide an output file path, and the results are saved as an Excel file.
Align_dates <- function(df) {
  
  # Helper function to infer seasons from date_time
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
  
  # Load official record
  official_record <- read.csv("https://utexas.box.com/shared/static/9hcctqqilisc0t61wbbdiziig8ok8rg8.csv")
  
  # Add year and season columns
  df$year_date_time <- year(ymd_hms(df$date_time))
  df$season_date_time <- infer_season_date_time(df$date_time)
  official_record$season_date_time <- infer_season_date_time(ymd_hms(official_record$date_time))
  official_record$year_date_time <- year(ymd_hms(official_record$date_time))
  
  # Use check_data_matches to compare input data with official record
  results_data <- check_data_matches(df, official_record)
  
  # Track already-printed messages to avoid duplicates
  printed_messages <- c()
  
  # Update the date_time column in the input dataframe and display mismatch messages
  for (i in seq_len(nrow(results_data))) {
    row <- results_data[i, ]
    
    # Generate the message
    message_text <- NULL
    if (row$match_status == "No station match") {
      message_text <- paste("No Station match:", row$station)
    } else if (row$match_status == "Station match but no year match") {
      message_text <- paste("Station match but no year match:", row$station, 
                            "Original year:", df$year_date_time[i])
    } else if (row$match_status == "Year match but no season match") {
      message_text <- paste("Year match but no season match:", row$station, 
                            "Original season:", df$season_date_time[i])
    } else if (grepl("Multiple dates found in CP data", row$match_status)) {
      message_text <- paste("Multiple dates found for station", row$station, 
                            "in CP data:", row$match_status)
    }
    
    # Print the message only if it hasn't been printed before
    if (!is.null(message_text) && !(message_text %in% printed_messages)) {
      message(message_text)
      printed_messages <- c(printed_messages, message_text) # Add to the printed set
    }
    
    # Update the date_time if needed
    if (!is.na(row$expected_date_time) && row$original_date_time != row$date_time_changed) {
      df$date_time[df$station == row$station & df$date_time == row$original_date_time] <- row$date_time_changed
    }
  }
  
  # Remove columns
  df <- df %>% select(-year_date_time, -season_date_time)
  
  # Return the updated dataframe
  return(df)
}