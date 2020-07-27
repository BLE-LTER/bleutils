#' @title Order Core Program data columns
#' @description Order Core Program data columns in this particular order: node, lagoon, station, season, date_collected/date_time, water_column_position, (insert data columns), station_name, latitude, longitude, habitat_type, station_sampling_priority.
#' @param data (data.frame) A data.frame containing Core Program data. Columns names need to be named according to standard CP column names. Use bleutils::reaname_attributes for this to work.
#' @param type (character) Type of CP data. Choose from "water", "sediment", or "mooring". This affects the columns present.
#'
#' @importFrom dplyr arrange %>%
#' @export
#'

order_cp_cols <- function(data, type){
  begin <- c("node",
             "lagoon",
             "station",
             if (type %in% c("water", "sediment")) c("season", "date_collected"),
             if (type == "mooring") "date_time",
             if (type == "water") "water_column_position")
  end <- c(
    if (type == "water") "collection_method",
        "station_name", "latitude", "longitude", "habitat_type", "station_sampling_priority")

  data_cols <- setdiff(colnames(data), c(begin, end))

  data <- subset(data, select = c(begin, data_cols, end))
  return(data)
}
