#' Add columns relevant to BLE LTER Core Program
#' Add columns: station name, lat/lon, habitat type, given BLE LTER Core Program station code
#' @param df (data.frame) Data frame in question with CP data
#' @param station_code_col (character) Name of column containing BLE LTER station codes in supplied df. Defaults to "station".
#' @return Data with new station name, lat/lon, habitat type columns appended
#' @export

add_cp_cols <- function(df, station_code_col = "station") {
  # check existing data for columns first before merging
  cols <- c(setdiff(colnames(station_info), colnames(df)))
  if ("station_id" %in% cols) {
    cols = cols
  } else {
    cols <- c(cols, "station_id")
  }

  # merge
  df <- merge(df, station_info[cols], by.x = station_code_col, by.y = "station_id")
  return(df)
}
