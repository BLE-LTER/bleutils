#' @title Sort Core Program data
#' @description Sort Core Program data in preferred sort order: node, lagoon, station, date/date_time, then water_column_position if applicable
#'@param data (data.frame) A data.frame containing Core Program data. Columns already need to be ordered in the particular CP order. Use bleutils::order_cp_cols for this. Columns names also need to be named according to standard CP column names. Use bleutils::reaname_attributes for this.
#' @param type (character) Type of CP data. Choose from "water", "sediment", or "mooring". This affects the columns on which to sort data.
#'
#' @return (data.frame) The same data but ordered.
#'
#' @export

sort_cp_data <- function(data, type){
  stopifnot(c("node", "lagoon", "station") %in% colnames(data))
 sort_by <- order(data[["node"]],
                  data[["lagoon"]],
                  data[["station"]])
  if (type == "water") {
    sort_by <- order(data[["node"]],
                     data[["lagoon"]],
                     data[["station"]],
                     data[["date_collected"]],
                     data[["water_column_position"]])
  } else if (type == "sediment") {
    sort_by <- order(data[["node"]],
                     data[["lagoon"]],
                     data[["station"]],
                     data[["date_collected"]])
  } else if (type == "mooring") {
    sort_by <- order(data[["station"]],
                     data[["date_time"]])
  }
 data <- data[sort_by, ]
  return(data)
}
