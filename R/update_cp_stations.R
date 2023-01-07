#' #' Compare the station information to "canonical" source in Box and update if needed. Meant to be run occassionally from An's PC and within the bleutils Rproject to update the version of station info that this package uses to append new columns, for example.
#' #'
#' #' @param source_file (character) Direct URL to file with station codes, names, lat/lons, habitat type. Defaults to "https://utexas.box.com/shared/static/p6a8956afbsfh19422p8xhhtiqmf7eqy.csv" hosted on the "BLE-IM" folder on Box.
#' #'
#' #' @export
#'
#' update_cp_stations <-
#'   function(source_file = "https://utexas.box.com/shared/static/p6a8956afbsfh19422p8xhhtiqmf7eqy.csv") {
#'     try(canon <- read.csv(source_file))
#'     # canon <- readxl::read_excel(source_file, sheet = sheet)
#'     if (exists("stations", where = "package:bleutils") &&
#'         isTRUE(all.equal(bleutils::stations, canon)) &&
#'         all(colnames(station_info) == colnames(canon)))
#'       message("Package version of station info is up-to-date. No action taken.")
#'     else {
#'       message("Nope. Something's changed. Updating package version of station info.")
#'       stations <- canon
#'       if (#package environment
#'       ) {
#'         usethis::use_data(stations, overwrite = TRUE)
#'       } else if () {
#'         save(stations.)
#'       }
#'
#'       # save(stations, "stations.rda")
#'       # message("stations have been updated at path:.... Please update on github")
#'     }
#'   }
