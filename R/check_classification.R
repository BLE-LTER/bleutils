#' Fetches taxonomic classification for a given AphiaID from the WoRMS API.
#' @description This function queries the WoRMS API to retrieve taxonomic classification information for a specified AphiaID. If the AphiaID is NA or the fetch fails, it returns NULL with a warning.The function handles HTTP response errors by warning the user and returns the classificationas a list if successful.
#' @param aphia_id An integer representing the AphiaID for which to fetch the classification.
#' @return Returns a list containing the taxonomic levels (Kingdom, Phylum, Class, Order, Family, Genus, Species) of the classification if successful. Returns NULL if the fetch is unsuccessful or if the AphiaID is NA.
#' @import httr jsonlite
#' Checks classification mismatches between a dataframe and the WoRMS database
#' @description This function verifies the taxonomic classification of species in a dataframe against data from the WoRMS API based on AphiaID. It iterates over each unique AphiaID in the dataframe, fetches the corresponding classification from WoRMS, and compares it against the dataframe values. Mismatches are collected and printed. If no dataframe is provided or the input is not a dataframe,the function will stop with an error.
#' @param input_dataframe A dataframe with at least one column named `aphia_ID` and other columns corresponding to taxonomic levels (e.g., kingdom, phylum, class, order, family, genus, species).
#' @return Returns a list of mismatches, where each mismatch is a list containing the AphiaID and the discrepancies between the CSV data and the WoRMS data. Each mismatch specifies the CSV value and the WoRMS value for each differing taxonomic level.

# Function to fetch classification from WoRMS
fetch_classification_from_worms <- function(aphia_id) {
  if (is.na(aphia_id)) {
    warning("AphiaID is NA, skipping fetch.")
    return(NULL)
  }
  
  url <- paste0("https://www.marinespecies.org/rest/AphiaRecordByAphiaID/", aphia_id)
  response <- GET(url)
  
  # Error handling for the response
  if (http_type(response) != "application/json" || http_status(response)$category != "Success") {
    warning(paste("Failed to fetch data for AphiaID:", aphia_id, "-", http_status(response)$message))
    return(NULL)
  }
  
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

check_classification <- function(input_dataframe) {
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
        for (level in names(mismatches[[aphia_id]])) {
          cat("  ", level, ":\n")
          cat("    CSV:   ", mismatches[[aphia_id]][[level]]$CSV, "\n")
          cat("    WoRMS: ", mismatches[[aphia_id]][[level]]$WoRMS, "\n")
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
    mismatch <- list()
    
    for (level in names(worms_classification)) {
      if (!is.null(worms_classification[[level]]) && worms_classification[[level]] != "") {
        csv_values <- as.character(csv_rows[[tolower(level)]])
        worms_value <- as.character(worms_classification[[level]])
        
        if (level == "Species") {
          worms_value <- paste(worms_classification$Species)
          csv_values <- paste(csv_rows$genus, csv_rows$species)
        }

        for (csv_value in csv_values) {
          if (tolower(csv_value) != tolower(worms_value)) {
            mismatch[[level]] <- list(CSV = csv_value, WoRMS = worms_value)
          }
        }
      }
    }
    
    if (length(mismatch) > 0) {
      mismatches[[as.character(aphia_id)]] <- mismatch
    }
    
    Sys.sleep(0.5)
  }
  
  print_mismatches(mismatches)
  return(mismatches)
}