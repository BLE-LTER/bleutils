#' @title Sort Core Program data
#' @description Sort Core Program data in preferred sort order: node, lagoon, station, date/date_time, then water_column_position if applicable
#'@param data (data.frame) A data.frame containing Core Program data. Columns already need to be ordered in the particular CP order. Use \code{bleutils::order_cp_cols} for this. Columns names also need to be named according to standard CP column names. Use \code{bleutils::rename_attributes} for this.
#' @param type (character) Type of CP data. Choose from "water", "sediment", or "mooring". This affects the columns on which to sort data. "water" sorts data by node, lagoon, station, date_time, water_column_position. "sediment" by node, lagoon, station, date_time. Use "sediment" for biota data as well. "mooring" sorts data by station, date_time.
#'
#' @return (data.frame) The same data but ordered.
#'
#' @export

sort_cp_rows <- function(data, type) {
  stopifnot(c("node", "lagoon", "station") %in% colnames(data))
  
  # Define columns to sort by based on type
  sort_columns <- switch(
    type,
    "water" = c("node", "lagoon", "station", "date_time", "water_column_position", "wavelength_nm"),
    "sediment" = c("node", "lagoon", "station", "date_time"),
    "mooring" = c("station", "date_time"),
    stop("Invalid type")
  )
  
  # Filter sort_columns to only include existing columns in the dataframe
  sort_columns <- intersect(sort_columns, colnames(data))
  
  # Create vector of columns to sort
  sort_by <- do.call("order", data[sort_columns])
  
  # Sort the dataframe
  data <- data[sort_by, ]
  
  return(data)
}