#' Add columns relevant to BLE LTER Core Program
#' Add columns: station name, lat/lon, habitat type, given BLE LTER Core Program station code
#' @param df (data.frame) Data frame in question with CP data
#' @param station_code_col (character) Name of column containing BLE LTER station codes in supplied df. Defaults to "station".
#' @return Data with new station name, lat/lon, habitat type columns appended
#' @export

add_cp_cols <- function(df, station_code_col = "station") {
  # TODO: check existing data for columns first before merging

  df <- merge(df, bleutils::station_info, by.x = station_code_col, by.y = "station_id")
  return(df)
}

#' Compare the station information to "canonical" source in Box and update if needed. Meant to be run occassionally from An's PC and within the bleutils Rproject to update the version of station info that this package uses to append new columns, for example.
#' @param source_file (character) Full path to canonical file used to store station codes, names, lat/lons, habitat type. Defaults to "C:/Users/atn893/Documents/Box Sync/Beaufort LTER/Core Program/BLE_LTER_CP_Stations.xlsx" which works on An's PC.
#' @param sheet (character) Name of Excel sheet containing lookup info. Defaults to "lookup".
#' importFrom readxl read_excel
#' @export

update_cp_stations <- function(source_file = "C:/Users/atn893/Documents/Box Sync/Beaufort LTER/Core Program/BLE_LTER_CP_Stations.xlsx",
                               sheet = "lookup") {
  canon <- readxl::read_excel(source_file, sheet = sheet)
  if (exists("station_info", where = "package:bleutils") && isTRUE(all.equal(bleutils::station_info, canon)) && all(colnames(station_info) == colnames(canon))) message("Package version of station info is up-to-date. No action taken.")
  else {
    message("Nope. Something's changed. Updating package version of station info.")
    station_info <- canon
    usethis::use_data(station_info, overwrite = TRUE)
  }
}
