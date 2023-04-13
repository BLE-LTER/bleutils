#' @title Connect to metabase and query for personnel list.
#'
#' @description Connect to instance of LTER-core-metabase and query for personnel list with years associated (BLE LTER Core Program data package requirement).
#'
#' @param dbname (character) name of database. Defaults to "ble_metabase".
#' @param schema (character) name of schema containing views. Defaults to 'pkg_mgmt'.
#' @param dataset_ids (numeric) Number or numeric vector of dataset IDs to query.
#' @param host (character) host name or IP address. Defaults to 'localhost'.
#' @param port (numeric) port number. Defaults to 5432.
#' @param user (character) (optional) username to use in connecting to database. Use to save time. If not supplied, the R console will prompt you to enter a username.
#' @param password (character) (optional) password to user. Use to save time. If not supplied, the R console will prompt you to enter a password.
#' @param write_to_file (logical) Whether to write to file. Defaults to TRUE.
#' @param file_dir (character) Full path of directory to write file to. Defaults to current working directory.
#' @param file_name (character) File name, will default to "BLE_LTER_(dataset_ids)_personnel.csv" if not specified.
#' @return (data.frame) A data frame of personnel associated with specified dataset IDs as queried from metabase.
#' @export

export_personnel <-
  function(dbname = "ble_metabase",
           schema = "pkg_mgmt",
           dataset_ids,
           host = "localhost",
           port = 5432,
           user = NULL,
           password = NULL,
           write_to_file = TRUE,
           file_dir = getwd(),
           file_name = NULL)
  {
    driver <- RPostgres::Postgres()
    con <- DBI::dbConnect(
      drv = driver,
      dbname = dbname,
      host = host,
      port = port,
      user = if (is.null(user))
        readline(prompt = "Enter database username: ")
      else
        user,
      password = if (is.null(password))
        readline(prompt = "Enter database password: ")
      else
        password
    )
    view_to_query <-
      paste0(schema, ".", "personnel_years_associated")
    query <-
      paste("SELECT * FROM", view_to_query, "WHERE datasetid = $1")
    result <- RPostgres::dbSendQuery(conn = con, query)
    RPostgres::dbBind(result, list(dataset_ids))
    query_df <- RPostgres::dbFetch(result)
    RPostgres::dbClearResult(result)
   DBI::dbDisconnect(con)

    query_df[["datasetid"]] <-
      paste0("knb-lter-ble.", query_df[["datasetid"]])
    query_df[is.na(query_df[["middlename"]]), "middlename"] <- ""
    if (write_to_file) {
      if (is.null(file_name)) file_name <- paste0(
        "BLE_LTER_",
        paste0(dataset_ids, collapse = "_"),
        "_personnel.csv"
      )
      write.csv(query_df,
                file = file.path(file_dir, file_name),
                row.names = FALSE)
    }
    message("You might want to erase command history, since user password to your database was given.")
    return(query_df)
  }
