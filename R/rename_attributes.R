#' @title Rename data columns according to metadata.
#'
#' @description Rename data columns according to metadata.
#'
#' @param meta_list (character) A list of dataframes containing metadata returned by \code{\link{get_meta}}.
#' @param dataset_id (numeric) A dataset ID.
#' @param entity (numeric) An entity number.
#' @param file_dir (character) Path to directory containing flat files (data files). Defaults to current R working directory.
#' @param file_name (character) File name to
#' @param x R object. If specified, will skip looking for entity file altogether.

#' @param append_units (logical) If TRUE, will append abbreviated units (see vw_custom_units) to column names as specified in metabase. Defaults to TRUE.
#'
#' @return The data table entity with renamed columns.
#' @export


rename_attributes <-
  function(meta_list,
           dataset_id,
           entity,
           file_dir = getwd(),
           file_name = "",
           x,
           append_units = T) {
    if (!is.null(x))
      stopifnot(is.data.frame(x))

    # subset to specified dataset_id and entity number
    entity_e <-
      subset(meta_list[["entities"]], datasetid == dataset_id &
               entity_position == entity)

    # convert whitespace strings to NA for easy checking
    entity_e <- lapply(entity_e, stringr::str_trim)
    entity_e[entity_e == ""] <- NA
    entity_e <- as.data.frame(entity_e)

    attributes <-
      subset(meta_list[["attributes"]], datasetid == dataset_id &
               entity_position == entity)
    if (is.null(x)) {
      if (file_name != "") {
        entity_df <- data.table::fread(file.path(file_dir, file_name))
      } else
        entity_df <-
          data.table::fread(file.path(file_dir, entity_e[["filename"]]))
    } else
      entity_df <- x

    colnames(entity_df) <- attributes[["attributeName"]]
    return(entity_df)
  }
