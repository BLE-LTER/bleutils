#' Fetches taxonomic classification for a given AphiaID from WoRMS
#' This function queries the WoRMS API to retrieve taxonomic classification information for a specified AphiaID. If the AphiaID is NA or the fetch fails, it returns NULL with a warning.
#' The function handles HTTP response errors by warning the user and returns the classification as a list if successful.
#' @param aphia_id An integer representing the AphiaID for which to fetch the classification.
#' @return Returns a list containing the taxonomic levels (Kingdom, Phylum, Class, Order, Family, Genus, Species) of the classification if successful. Returns NULL if the fetch is unsuccessful or if the AphiaID is NA.
#' @import httr jsonlite
#' @export

fetch_classification_from_worms <- function(aphia_id) {
  library(httr)
  library(jsonlite)
  
  url <- paste0("https://www.marinespecies.org/rest/AphiaRecordByAphiaID/", aphia_id)
  response <- GET(url)
  data <- content(response, "text", encoding = "UTF-8")
  json_data <- fromJSON(data, flatten = TRUE)
  
  classification <- list(
    Kingdom = json_data$kingdom,
    Phylum = json_data$phylum,
    Class = json_data$class,
    Order = json_data$order,
    Family = json_data$family,
    Genus = json_data$genus,
    Species = json_data$scientificname
  )
  
  return(classification)
}

#' Checks classification mismatches between a dataframe and the WoRMS database
#' This function verifies the taxonomic classification of species in a dataframe against data from the WoRMS API based on AphiaID. It iterates over each unique AphiaID in the dataframe, fetches the corresponding classification from WoRMS, and compares it against the dataframe values. 
#' Mismatches are collected and printed. If no dataframe is provided or the input is not a dataframe,the function will stop with an error.
#' @param input_dataframe A dataframe with at least one column named `aphia_ID` and other columns corresponding to taxonomic levels (e.g., kingdom, phylum, class, order, family, genus, species).
#' @return Returns a list of mismatches, where each mismatch is a list containing the AphiaID and the discrepancies between the CSV data and the WoRMS data. Each mismatch specifies the CSV value and the WoRMS value for each differing taxonomic level.

check_classification <- function(input_dataframe) {
  library(httr)
  library(jsonlite)
  
  if (!is.data.frame(input_dataframe)) {
    stop("The input is not a dataframe.")
  }
  
  # Function to print mismatches
  print_mismatches <- function(mismatches) {
    if (length(mismatches) == 0) {
      cat("No mismatches found.\n")
    } else {
      for (aphia_id in names(mismatches)) {
        cat("Mismatches for AphiaID", aphia_id, ":\n")
        for (mismatch in mismatches[[aphia_id]]) {
          cat("  Excel Row", mismatch$excel_row, ":\n")
          for (level in names(mismatch$discrepancies)) {
            cat("    ", level, ":\n")
            cat("      CSV:   ", mismatch$discrepancies[[level]]$CSV, "\n")
            cat("      WoRMS: ", mismatch$discrepancies[[level]]$WoRMS, "\n")
          }
        }
      }
    }
  }

  aphia_ids <- unique(input_dataframe$aphia_ID)
  mismatches <- list()
  
  for (aphia_id in aphia_ids) {
    if (is.na(aphia_id)) {
      warning("Skipping NA AphiaID.")
      next
    }
    
    retries <- 3
    worms_classification <- NULL
    while (is.null(worms_classification) && retries > 0) {
      worms_classification <- fetch_classification_from_worms(aphia_id)
      retries <- retries - 1
      Sys.sleep(1)
    }
    
    if (is.null(worms_classification)) {
      warning(paste("Skipping AphiaID", aphia_id, "due to repeated fetch failures."))
      next
    }
    
    csv_rows <- input_dataframe[input_dataframe$aphia_ID == aphia_id, ]
    for (row_index in seq_along(csv_rows[, 1])) {
      row_number <- which(input_dataframe$aphia_ID == aphia_id)[row_index] + 1  # Assuming header in row 1
      mismatch <- list(discrepancies = list(), excel_row = row_number)
      
      for (level in names(worms_classification)) {
        #print (level)
        if (!is.null(worms_classification[[level]]) && worms_classification[[level]] != "") {
          csv_value <- as.character(csv_rows[row_index, tolower(level)])
          worms_value <- as.character(worms_classification[[level]])    
          if((is.na(csv_value)) || (is.na(worms_value))) {
            next
          }
          if(level == "Species") {
            csv_value1 <- as.character(csv_rows[row_index, "genus"]) # added to check for concatenated values
            csv_value2 <- as.character(csv_rows[row_index, tolower(level)])
            csv_value <- paste(csv_value1, csv_value2)
            
          }
          
          
          if (tolower(csv_value) != tolower(worms_value)) {
             #cat ("worms", worms_value, "\n")
             #cat ("excel", csv_value, "\n")
            mismatch$discrepancies[[level]] <- list(CSV = csv_value, WoRMS = worms_value)
          }
        }
      }
      
      if (length(mismatch$discrepancies) > 0) {
        mismatches[[as.character(aphia_id)]] <- c(mismatches[[as.character(aphia_id)]], list(mismatch))
      }
    }
    
    Sys.sleep(0.5)
  }
 
 print_mismatches(mismatches)
  return(mismatches)
}