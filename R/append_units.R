#' @title Append units to column names
#' @description Append abbreviated units to attribute names
#' @param meta_list (list) A metadata list structure returned by MetaEgress::get_meta()
#' @param skip (numeric) Vector of entity numbers to exclude from appending units. E.g. raw data entities might need to be archived as-is.
#'
#' @export


append_units <- function(meta_list, skip = NULL) {

  attributes <-
    subset(meta_list[["attributes"]])
  factors <- subset(meta_list[["factors"]])
  missing <- subset(meta_list[["missing"]])
  units <- subset(meta_list[["unit"]], select = c(id, abbreviation))
  attributes <-
    dplyr::left_join(attributes, units, by = c("unit" = "id"))

  for (i in 1:nrow(attributes)) {
    # skip if enity in skip list
    if (!attributes[["entity_position"]][[i]] %in% skip) {
      # skip if no abbreviation or if attribute is lat/lon. what if it's lat/lon but unit is not degree?
      if (!is.na(attributes[["abbreviation"]][[i]]) &
          !grepl("latitude|longitude", attributes[["attributeName"]][[i]], ignore.case = T)) {
        old_name <- attributes[["attributeName"]][[i]]
        new_name <-
          paste(attributes[["attributeName"]][[i]], attributes[["abbreviation"]][[i]], sep = "_")
        attributes[["attributeName"]][[i]] <- new_name
        factors[["attributeName"]][factors[["attributeName"]] == old_name] <-
          new_name
        missing[["attributeName"]][missing[["attributeName"]] == old_name] <-
          new_name
      }
    }
  }
  meta_list[["attributes"]] <-
    subset(attributes, select = -abbreviation)
  meta_list[["factors"]] <- factors
  meta_list[["missing"]] <- missing
  return(meta_list)
}
