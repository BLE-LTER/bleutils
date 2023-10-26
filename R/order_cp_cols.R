#' @title Order Core Program data columns
#' @description Order Core Program data columns in this particular order: node, lagoon, station, season, date_time, water_column_position, (insert data columns), station_name, latitude, longitude, station_depth, habitat_type, station_sampling_priority. Note that station_depth is optional.
#' @param data (data.frame) A data.frame containing Core Program data. Columns names need to be named according to standard CP column names. Use \code{bleutils::rename_attributes} prior to running this to rename columns to what's in metadata.
#' @param type (character) Type of CP data. Choose from "water", "sediment", or "mooring". Use "sediment" for biota data as well.
#'
#' @export

order_cp_cols <- function(data, type){
  begin <- c("node",
             "lagoon",
             "station",
             if (type %in% c("water", "sediment")) c("season", "date_time"),
             if (type == "mooring") "date_time",
             if (type == "water") "water_column_position")
  end <- c(
    if (type == "water") "collection_method",
    "station_name",
    "latitude",
    "longitude",
    if (type == "water" && "station_depth" %in% colnames(data)) "station_depth",
    "habitat_type",
    "station_sampling_priority")

  data_cols <- setdiff(colnames(data), c(begin, end))

  # Check for missing required columns
  missing_begin <- setdiff(begin, colnames(data))
  missing_end <- setdiff(end, colnames(data))
  missing_columns <- union(missing_begin, missing_end)
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing:", paste(missing_columns, collapse = ", ")))
  }

  data <- subset(data, select = c(begin, data_cols, end))
  return(data)
}
