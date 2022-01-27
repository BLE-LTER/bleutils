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

  # create sub-directories
  if (!dir.exists(file.path(dsdir, "FromPI")))
    dir.create(file.path(dsdir, "FromPI"))

  if (!dir.exists(file.path(dsdir, "Clean"))) {
    dir.create(file.path(dsdir, "Clean"))
  }
  if (!dir.exists(file.path(dsdir, paste0("EML_RProject_", dataset_id)))) {
    dir.create(file.path(dsdir, paste0("EML_RProject_", dataset_id)))
  }

  print(dsdir)
  # write README
  readme <- readLines(system.file("readme.txt", package = "bleutils"))
  writeLines(readme, file.path(dsdir, "README.txt"))

  message(paste("Created directory structure under", dsdir))

  # init data procressing script
  init_script(dataset_id = dataset_id, workdir = dsdir, type = "init")
}
