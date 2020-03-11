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
    stop(paste("Dataset ID already existing. Next available ID:", next_id))
  }

  dsdir <-
    file.path(base_path,
              paste0(dataset_id, "_", dataset_nickname))
  if (!dir.exists(dsdir))
    dir.create(dsdir)
  if (!dir.exists(file.path(dsdir, "FromPI")))
    dir.create(file.path(dsdir, "FromPI"))
  workdir <- file.path(dsdir, "Clean")
  if (!dir.exists(workdir)) {
    dir.create(workdir)
    dir.create(file.path(workdir, "EML_generation"))
    dir.create(file.path(workdir, "EML_generation", "data"))
  }
  message(paste("Created directory structure under", dsdir))

  init_script(dataset_id = dataset_id, workdir = workdir)
}

#' Initiate processing script from template
#'
#'

init_script <- function(dataset_id, workdir) {
  template <-
    readLines(system.file("template.R", package = "bleutils"))
  template <- gsub("datasetid", dataset_id, template)
  script_name <- paste0("dataset", dataset_id, ".R")
  writeLines(template, file.path(workdir, "EML_generation", script_name))
  message(paste(
    "Created from template processing script",
    script_name,
    "under",
    file.path(workdir, "EML_generation")
  ))
}

