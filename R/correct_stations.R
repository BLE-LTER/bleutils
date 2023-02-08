#' Correct station codes on certain data rows based on dates sampled.
#' @description The need for this function arose when in 2023 it was determined that in the past some stations were not labelled correctly during break-up season (june/july). Due to ice conditions, on certain dates sampling was done not at the station (within 100m of waypoint coordinates). This function takes a csv of corrections of dates, old and new station codes, and convert the old codes to new only on these dates. Note that this removes the station information (coordinates, etc) and one can add it back with add_cp_cols.
#' @param df (data.frame) Data.frame of data with stations to be corrected
#' @param station_code_col (character) Name of column containing station codes
#' @param corrections_source (character) URL to corrections CSV file. I use a shared Box file for this.
#'
#' @return
#' @export
#'
correct_stations <-
  function(df,
           station_code_col = "station",
           corrections_source = "https://utexas.box.com/shared/static/1uy9n875jtc7djxlckm0ogv6v3mwes6f.csv") {
    # remove standard columns so we can add them later
    df2 <-
      df[, !colnames(df) %in% c(
        "station_name",
        "latitude",
        "longitude",
        "station_sampling_priority",
        "habitat_type",
        "node",
        "lagoon"
      )]

    # ensure date times will merge
    if ("date_time" %in% colnames(df2)) {
      df2[["date_time"]] <- as.Date(df2[["date_time"]])
      date_col <- "date_time"
    } else {
      df2[["date_collected"]] <- as.Date(df2[["date_collected"]])
      date_col <- "date_collected"
    }
    corrections <-
      read.csv(corrections_source,
               colClasses = c("date" = "Date"))
    # merge based on dates and stations
    df2 <-
      merge(
        df2,
        corrections,
        by.x = c(date_col, "station"),
        by.y = c("date", "old_station"),
        all.x = T
      )
    # if there's a correction station name, take that one otherwise leave alone
    df2[["station"]] <-
      ifelse(!is.na(df2[["new_station"]]),
                    df2[["new_station"]],
                    df[["station"]])
    df2 <- df2[, !colnames(df2) %in% c("new_station")]
    return(df2)
  }
