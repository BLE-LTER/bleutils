

#' Generate R processing script from template
#' @param dataset_id (numeric) Dataset ID that the generated script will process
#' @param file_dir (character) Directory to save script to. Defaults to the respective dataset's directory on An's machine if not specified.
#' @param type (character) either "init" or "update" for new or returning data packages respectively. Defaults to "update". The difference is in the contents of the script. Returning data packages get a script with a section where the data from the last revision on EDI's production is downloaded and used as the basis for appending new data onto.
#' @param data_year (numeric) The year of data that the generated script will process. E.g. you may be working with 2021 data in 2022. Defaults to the current calendar year if not specified.
#'
#' @export

init_script <-
  function(dataset_id,
           file_dir = NULL,
           type = "update",
           data_year = NULL) {
    # data year is current year if not specified
    if (is.null(data_year))
      data_year <- as.integer(format(Sys.Date(), "%Y"))
    stopifnot(is.numeric(dataset_id), is.integer(data_year))

    # construct the default path if file_dir is NULL
    if (is.null(file_dir)) {
      file_dir <-
        paste0(
          "K:/Data/",
          list.files("K:/Data", pattern = paste0("^", dataset_id, "_"))[[1]],
          "/EML_RProject_",
          dataset_id,
          "/",
          data_year
        )
    }
    if (type == "init") {
      template <-
        readLines(system.file("template_init.R", package = "bleutils"))
    } else if (type == "update") {
      template <-
        readLines(system.file("template_update.R", package = "bleutils"))
    } else {
      stop("type needs to be init or update")
    }
    template <- gsub("datasetid", dataset_id, template)
    script_name <-
      paste0("dataset", dataset_id, "_", format(Sys.Date(), "%Y%m"), ".R")
    writeLines(template, file.path(file_dir, script_name))

    message(paste(
      "Created from template: script",
      script_name,
      "under directory",
      file_dir
    ))
  }
