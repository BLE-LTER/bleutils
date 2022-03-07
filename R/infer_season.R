#' @title Infer BLE LTER field campaign season from date or date-times
#' @description Infer BLE LTER field campaign season from date or date-times: dates in May or earlier is under ice, June to 15th of July is break up, and 16th of July or later is open water. Do not use with mooring data.
#' @param data data.frame A date.frame with date
#' @param date_col character Name of column with dates or date-times. Please convert this column into a Date format first (not just strings containing dates).
#' @param format for
#' @return A vector of strings: field campaign seasons in same order as dates or date-times supplied.
#' @import lubridate
#' @export

infer_season <-
  function(data,
           date_col = "date_time") {
    datetimes <- data[[date_col]]
    # stopifnot(lubridate::is.Date(datetimes), "Please convert the column into date or datetime format first.")

    months <- lubridate::month(datetimes)
    days <- lubridate::mday(datetimes)
    seasons <-
      ifelse(months <= 5,
             "under ice",
             ifelse(
               months == 6,
               "break up",
               ifelse(
                 months >= 8,
                 "open water",
                 ifelse(days > 15,
                        "open water",
                        "break up")
               )
             ))
    return(seasons)
  }
