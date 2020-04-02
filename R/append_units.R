#'
#'
#'
#'


append_units <- function(meta_list) {
  attributes <-
    subset(meta_list[["attributes"]])
  factors <- subset(meta_list[["factors"]])
  missing <- subset(meta_list[["missing"]])
  units <- subset(meta_list[["unit"]], select = c(id, abbreviation))
  attributes <-
    dplyr::left_join(attributes, units, by = c("unit" = "id"))
  for (i in 1:nrow(attributes)) {
    if (!is.na(attributes[["abbreviation"]][[i]])) {
      old_name <- attributes[["attributeName"]][[i]]
      new_name <- paste(attributes[["attributeName"]][[i]], attributes[["abbreviation"]][[i]], sep = "_")
      attributes[["attributeName"]][[i]] <- new_name
      factors[["attributeName"]][factors[["attributeName"]] == old_name] <- new_name
      missing[["attributeName"]][missing[["attributeName"]] == old_name] <- new_name
    }
  }
  meta_list[["attributes"]] <- subset(attributes, select = -abbreviation)
  meta_list[["factors"]] <- factors
  meta_list[["missing"]] <- missing
  return(meta_list)
}
