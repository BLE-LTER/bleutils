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
  
  tryCatch({
    json_data <- fromJSON(data, flatten = TRUE)
  }, error = function(e) {
    cat("JSON parsing error:", e$message, "\n")
    return(NULL)  # Return NULL on error
  })
  if (is.null(json_data)) {
  return(NULL)
  }
  
  classification <- list(
    Kingdom = json_data$kingdom,
    Phylum = json_data$phylum,
    Class = json_data$class,
    Order = json_data$order,
    Family = json_data$family,
    Genus = json_data$genus,
    Species <- ifelse(json_data$rank == "Species",
                      json_data$scientificname,
                      ifelse(json_data$rank == "Subspecies" && !is.null(json_data$parentNameUsageID) && json_data$parentNameUsageID != "",
                         {
                          parent <- fetch_classification_from_worms(json_data$parentNameUsageID)
                          species <- parent[['Species']]
                          if (!is.null(species) && !is.na(species) && species != "") {
                            species
                          } else {
                             NA  # Return NA if parent data is null or missing scientific name
                          }
                         },
                         NA)),

    taxa_id = json_data$scientificname
  )
  
  return(classification)
}

#' Checks classification mismatches between a dataframe and the WoRMS database
#' This function verifies the taxonomic classification of rows in a dataframe against data from the WoRMS API based on AphiaID. It iterates over each unique AphiaID in the dataframe, fetches the corresponding classification from WoRMS, and compares it against the dataframe values. 
#' Mismatches are collected and printed. If no dataframe is provided or the input is not a dataframe, the function will stop with an error.
#' @param input_dataframe A dataframe with at least one column named `aphia_ID` and other columns corresponding to taxonomic levels (kingdom, phylum, class, order, family, genus, species). Taxa levels columns must be lower case.
#' @return Returns a list of mismatches, where each mismatch is a list containing the AphiaID and the discrepancies between the dataframe and the WoRMS data. Each mismatch specifies the dataframe value and the WoRMS value for each differing taxonomic level.
#' @import httr jsonlite
#' @export
check_classification <- function(input_dataframe) {
  library(httr)
  library(jsonlite)
  
  if (!is.data.frame(input_dataframe)) {
    stop("The input is not a dataframe.")
  }
  
  # Open a connection to a text file where output will be written
  #sink("C:/Users/im23237/Documents/taxanew.txt")
  file_path <- readline(prompt = "Please enter the file path to save the output (e.g., C:/Users/YourName/Documents/output.txt): ")
    
    # Check if the user provided a file path
    if (file_path == "") {
      stop("No file path provided. Please enter a valid file path.")
    }
  
  sink(file_path)
  
  # Function to print mismatches
  print_mismatches <- function(mismatches) {
    if (length(mismatches) == 0) {
      cat("No mismatches found.\n")
    } else {
      for (aphia_id in names(mismatches)) {
        mismatch <- mismatches[[aphia_id]]

        flag <- 0
       
        
        cat("Mismatches for AphiaID", aphia_id,  ":\n")
        cat("   scientific name ", mismatch$gs, mismatch$ss, "\n")
        cat("     Example Excel Row", mismatch$excel_row, ":\n")
        
        for (level in names(mismatch$discrepancies)) {
          if(flag == 0 ){
      
        flag <- flag + 1
        }
                  cat("    ", level, ":\n")
          cat("      CSV:   ", mismatch$discrepancies[[level]]$CSV, "\n")
          cat("      WoRMS: ", mismatch$discrepancies[[level]]$WoRMS, "\n")
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
    unique_mismatch_found <- FALSE
    
    for (row_index in seq_along(csv_rows[, 1])) {
      row_number <- which(input_dataframe$aphia_ID == aphia_id)[1] + 1  # Assuming header in row 1
      
       genus_value = as.character(csv_rows[row_index, "genus"])
          
            species_value = as.character(csv_rows[row_index, "species"])
      mismatch <- list(discrepancies = list(), excel_row = row_number,gs = genus_value, ss = species_value)
      
      for (level in names(worms_classification)) {
        csv_value <- as.character(csv_rows[row_index, tolower(level)])
        if (!is.null(worms_classification[[level]]) && !is.na(worms_classification[[level]]) && worms_classification[[level]] != "") {
          worms_value <- as.character(worms_classification[[level]])
          if (level == "Species") {
            csv_value <- paste(as.character(csv_rows[row_index, "genus"]), csv_value)
          }
          if (!is.na(csv_value) && !is.na(worms_value) && csv_value != worms_value) {
            
            mismatch$discrepancies[[level]] <- list(CSV = csv_value, WoRMS = worms_value)
       
          }
        } else {
          csv_column_name <- tolower(level)
          if (csv_column_name %in% names(csv_rows)) {
            csv_value <- as.character(csv_rows[row_index, csv_column_name])
            # Skip if both csv_value and worms_value are NA or empty
            worms_value <- ifelse(is.null(worms_classification[[level]]), NA, as.character(worms_classification[[level]]))
            if ((is.na(csv_value) || csv_value == "") && (is.na(worms_value) || worms_value == "")) {
              next
            }
                             
             # If we got here, then the WoRMS classification was null. If the CSV isn't NA, then we should add it to the mismatches.
            if (!is.na(csv_value) && csv_value != "") {
              genus_value = as.character(csv_rows[row_index, "genus"])
          
            species_value = as.character(csv_rows[row_index, "species"])
              mismatch$discrepancies[[level]] <- list(CSV = csv_value, WoRMS = "NA", genus = genus_value, species = species_value)
              
            }
            # If we got here, then the WoRMS classification has value. If the CSV is NA, then we should add it to the mismatches.
            if (is.na(csv_value) || csv_value == "") {
              genus_value = as.character(csv_rows[row_index, "genus"])
          
            species_value = as.character(csv_rows[row_index, "species"])
              mismatch$discrepancies[[level]] <- list(CSV = "NA", WoRMS = worms_classification[[level]],genus = genus_value, species = species_value)
              
          
            
            }
          }
        }
      }

      if (length(mismatch$discrepancies) > 0) {
        mismatches[[as.character(aphia_id)]] <- mismatch
        unique_mismatch_found <- TRUE
        break
      }
    }
    
    Sys.sleep(0.5)
  }
 
  print_mismatches(mismatches)
  sink()
  
    cat("Output saved to:", file_path, "\n")
  return(mismatches)
}