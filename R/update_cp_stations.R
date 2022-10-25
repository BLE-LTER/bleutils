#' Compare the station information to "canonical" source in Box and update if needed. Meant to be run occassionally from An's PC and within the bleutils Rproject to update the version of station info that this package uses to append new columns, for example.
#' @param source_file (character) Full path to canonical file used to store station codes, names, lat/lons, habitat type. Defaults to "C:/Users/atn893/Documents/Box Sync/Beaufort LTER/Core Program/BLE_LTER_CP_Stations.xlsx" which works on An's PC.
#' @param sheet (character) Name of Excel sheet containing lookup info. Defaults to "lookup".
#' @export

update_cp_stations <- function(source_file = "C:/Users/atn893/Documents/Box Sync/Beaufort LTER/Core Program/BLE_LTER_CP_Stations.xlsx",
                               sheet = "lookup") {
  canon <- readxl::read_excel(source_file, sheet = sheet)
  if (exists("stations", where = "package:bleutils") && isTRUE(all.equal(bleutils::stations, canon)) && all(colnames(station_info) == colnames(canon))) message("Package version of station info is up-to-date. No action taken.")
  else {
    message("Nope. Something's changed. Updating package version of station info.")
    stations <- canon
    usethis::use_data(stations, overwrite = TRUE)
  }
}
