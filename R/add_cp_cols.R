#' Add columns relevant to BLE LTER Core Program
#' @description Add columns: station name, lat/lon, habitat type, given BLE LTER Core Program station code
#'
#' @param df (data.frame) Data frame in question with CP data
#' @param station_source (character) URL or file path to Core Program stations coordinates file.
#' @param station_code_col (character) Name of column containing BLE LTER station codes in supplied df. Defaults to "station".
#'
#' @return Data with new station name, lat/lon, habitat type columns appended
#' @export

add_cp_cols <-
  function(df,
           station_code_col = "station",
           station_source = "https://utexas.box.com/shared/static/sjt5phkdpyx9vsvpvcss461562vb5wsw.csv") {
    # get stations from online Box file
    stations <- read.csv(station_source)
    # check existing data for columns first before merging
    cols <- c(setdiff(colnames(stations), colnames(df)))
    if ("station_id" %in% cols) {
      cols = cols
    } else {
      cols <- c(cols, "station_id")
    }

    # merge
    df <-
      merge(df, stations[cols], by.x = station_code_col, by.y = "station_id")
    return(df)
  }
