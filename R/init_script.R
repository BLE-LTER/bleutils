
#' Initiate processing script from template
#' @param dataset_id (numeric) Dataset ID
#' @param workdir (character)
#' @param type (character) either "init" or "update" for new or returning data packages respectively. The difference is in the contents of the script. Returning data packages get a script with a section where the data from the last revision on EDI's production is downloaded and used as the basis for appending new data onto.
#' @export

init_script <- function(dataset_id, workdir, type) {
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
  script_name <- paste0("dataset", dataset_id, "_", format(Sys.Date(), "%Y%m"), ".R")
  writeLines(template, file.path(workdir, paste0("EML_RProject_", dataset_id), script_name))
  message(paste(
    "Created from template: script",
    script_name,
    "under directory",
    file.path(workdir, paste0("EML_RProject_", dataset_id))
  ))
}

