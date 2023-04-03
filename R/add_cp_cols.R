#' Add columns relevant to BLE LTER Core Program
#' @description Add columns: station name, lat/lon, habitat type, given BLE LTER Core Program station code
#'
#' @param df (data.frame) Data frame in question with CP data
#' @param station_source (character) URL or file path to Core Program stations coordinates file. I use Box's direct download feature to generate URLs for this. The source file lives at https://utexas.app.box.com/file/1092468994724?s=sjt5phkdpyx9vsvpvcss461562vb5wsw.
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

    # check existing data for CP columns (e.g. if someone had already filled in the node column)
    cols <- c(setdiff(colnames(stations), colnames(df)))
    if (!"station_id" %in% cols) {
      cols <- c(cols, "station_id")
    }

    # warn if there are un-matched stations
    stations_df <- unique(df[[station_code_col]])
    stations_source <- unique(stations$station_id)

    if (!all(stations_df %in% stations_source)) {
      unmatched <- stations_df[!(stations_df %in% stations_source)]
      warning(paste0("There are stations in the supplied data frame that are not present in the reference source. They are: \n",
                    paste0(unmatched, collapse = "\n"),
                    "\n Rows with these unmatched stations have been retained with NAs in the additional columns. You may want to check the spelling of the station codes, for example: are Stefasson Sound stations still prefixed STD? We renamed those to SSL in 2022. For the reference source, see the Box file at https://utexas.app.box.com/file/1092468994724?s=sjt5phkdpyx9vsvpvcss461562vb5wsw. Contact An or Tim to add your unmatched stations to the reference source."))
    } else {
      message("All stations in the supplied data frame have matches in the reference source.")
    }

    # merge, keeping un-matched stations
    df <-
      merge(df,
            stations[cols],
            by.x = station_code_col,
            by.y = "station_id",
            all.x = TRUE)
    return(df)
  }
