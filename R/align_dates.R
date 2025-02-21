
#' Aligns Date and Time Data Based on Official Station Records
#'
#' This function compares date and time data from a user-provided dataframe (`df`) against an official record stored in a CSV file. It checks if the date-time for each station in the input dataframe matches the expected values from the official record, based on station, year, and season. The function attempts to correct mismatches where possible and reports any discrepancies. It updates the `date_time` column in the input dataframe where necessary and provides messages about the status of each match.
#'
#' @param df A dataframe containing station data with a `date_time` column. The dataframe must include a `station` column to match against the official record.
#' @param aligndates_source (character) URL to CSV file. I use a shared Box file for this. The source file is at https://utexas.box.com/shared/static/9hcctqqilisc0t61wbbdiziig8ok8rg8.csv 
#' @return A dataframe with the same structure as the input dataframe (`df`), but with updated `date_time` values where mismatches are corrected. The columns `year_date_time` and `season_date_time` are removed from the output.
#' 
#' @details
#' The function performs the following steps:
#' 1. Loads an official record CSV containing expected station data.
#' 2. Adds `year_date_time` and `season_date_time` columns to the input dataframe (`df`) and the official record using the `lubridate` package and the `infer_season` function.
#' 3. Compares the `date_time` column from `df` to the official record, checking for matches by station, year, and season.
#' 4. For each row, it outputs a message describing the match status (e.g., no match, partial match, or full match).
#' 5. Updates the `date_time` column in the input dataframe if a mismatch is found and a valid correction is possible.
#' 6. Returns the updated dataframe with corrected date-time values.
#' 
#' @import httr 
#' @import lubridate
#' @import dplyr
#'
#' @examples
#' # Assuming 'df' is a dataframe with a 'station' and 'date_time' column
#' aligned_df <- align_dates(df)
#'
#' @export
align_dates <- function(df) {
  

  # Internal function to check data matches
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

  # Load official record
  official_record <- read.csv("https://utexas.box.com/shared/static/9hcctqqilisc0t61wbbdiziig8ok8rg8.csv")

  # Add year and season columns
  df$year_date_time <- lubridate::year(lubridate::ymd_hms(df$date_time))
  df$season_date_time <- infer_season(data = df, date_col = "date_time")
  official_record$season_date_time <- infer_season(data = official_record, date_col = "date_time")
  official_record$year_date_time <- lubridate::year(lubridate::ymd_hms(official_record$date_time))

  # Use check_data_matches to compare input data with official record
  results_data <- check_data_matches(df, official_record)

  # Create an empty dataframe to store mismatch reports
  mismatch_report <- data.frame(
    Station = character(),
    Datetime_in_Dataset = character(),
    Comment = character(),
    stringsAsFactors = FALSE
  )

  # Track already-logged messages to avoid duplicates
  printed_messages <- c()

  for (i in seq_len(nrow(results_data))) {
    row <- results_data[i, ]

    # Construct message based on match status
    message_text <- NULL
    if (row$match_status == "No station match") {
      message_text <- c(row$station, row$original_date_time, "Station not found in lookup")
    } else if (row$match_status == "Station match but no year match") {
      message_text <- c(row$station, row$original_date_time, paste("Station in lookup, but not for", row$year_date_time))
    } else if (row$match_status == "Year match but no season match") {
      message_text <- c(row$station, row$original_date_time, paste("Station in lookup for this year, but not for season", df$season_date_time[i]))
    } else if (grepl("Multiple dates found in CP data", row$match_status)) {
      message_text <- c(row$station, row$original_date_time, "Multiple dates for this season found in lookup, but none match what's in the dataset")
    }

    # Add to the mismatch report if unique
    if (!is.null(message_text) && !(paste(message_text, collapse = ",") %in% printed_messages)) {
      mismatch_report <- rbind(mismatch_report, setNames(as.list(message_text), names(mismatch_report)))
      printed_messages <- c(printed_messages, paste(message_text, collapse = ","))
    }

    # Update the date_time column in df if correction is possible
    if (!is.na(row$expected_date_time) && row$original_date_time != row$date_time_changed) {
      df$date_time[df$station == row$station & df$date_time == row$original_date_time] <- row$date_time_changed
    }
  }

  # Print the structured mismatch report if there are discrepancies
  if (nrow(mismatch_report) > 0) {
    message("These stations in the dataset do not match datetimes in the CP station-datetime lookup table:")
    print(mismatch_report)
  }

  # Remove columns before returning the updated dataframe
  df <- df %>% dplyr::select(-year_date_time, -season_date_time)

  return(df)
}
