#' Create folder structure for a new BLE LTER data package ID
#' Create these following items: folder structure, R project, reusable R script from template
#' @param base_path (character) Full file path to parent directory to initiate under. Defaults to "K:/Data", which is "\\austin.utexas.edu/disk/engr/research/crwr/projects/BLE-LTER/Data" mapped to the K drive on An's PC.
#' @param dataset_id (numeric) Dataset ID
#' @param dataset_nickname (character)
#'
#' @return Nothing in R, but new folder structure and R script from template
#' @export

init_datapkg <- function(dataset_id,
                         dataset_nickname,
                         base_path = "K:/Data") {
  stopifnot(dataset_id %% 1 == 0)

  ## check for exisiting IDs
  dirs <- list.files(base_path)
  ids <- suppressMessages(as.integer(sapply(dirs, substr, 1, 1)))

  if (dataset_id %in% ids) {
    next_id <- max(ids, na.rm = T) + 1
    stop(paste("This dataset ID already exists. Next available ID:", next_id))
  }

  dsdir <-
    file.path(base_path,
              paste0(dataset_id, "_", dataset_nickname))

  if (!dir.exists(dsdir))
    dir.create(dsdir)

  if (!dir.exists(file.path(dsdir, "FromPI")))
    dir.create(file.path(dsdir, "FromPI"))

  if (!dir.exists(file.path(dsdir, "Clean"))) {
    dir.create(file.path(dsdir, "Clean"))
  }
  if (!dir.exists(file.path(dsdir, paste0("EML_RProject_", dataset_id)))) {
    dir.create(file.path(dsdir, paste0("EML_RProject_", dataset_id)))
  }

  message(paste("Created directory structure under", dsdir))

  init_script(dataset_id = dataset_id, workdir = dsdir, type = "init")
}

#' Initiate processing script from template
#' @param dataset_id (numeric) Dataset ID
#' @param workdir (character)
#' @param type (character) either "init" or "update" for new or returning data packages respectively. The difference is in the contents of the script. Returning data packages get a script with a section where the data from the last revision on EDI's production is downloaded and used as the basis for appending new data onto.

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

