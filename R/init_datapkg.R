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
  dir_path <-
    file.path(base_path,
              paste0("dataset", "_", dataset_id, "_", dataset_nickname))
  dir.create(dir_path)
  dir.create(file.path(dir_path, "FromPI"))
  dir.create(file.path(dir_path, "Cleaned_EML_generation"))
  message(paste("Created directory structure under", dir_path))

  template <- readLines(system.file("template.R", package = "bleutils"))
  template <- gsub("datasetid", dataset_id, template)
  script_name <- paste0("dataset", dataset_id, ".R")
  writeLines(template, file.path(dir_path, "Cleaned_EML_generation", script_name))
  message(paste("Created from template processing script", script_name, "under", file.path(dir_path, "Cleaned_EML_generation")))
}
