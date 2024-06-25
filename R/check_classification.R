fetch_classification_from_worms <- function(aphia_id) {
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

# Function to check classification
check_classification <- function(input_dataframe) {
  # Check if the input is a dataframe
  if (!is.data.frame(input_dataframe)) {
    stop("The input is not a dataframe.")
  }
  
  # Extract aphia_ids from the dataframe
  aphia_ids <- input_dataframe$aphia_ID
  unique_aphia_ids <- unique(input_dataframe$aphia_ID)
  # Initialize an empty list to store mismatches
  mismatches <- list()
  
  # Iterate through each AphiaID
  for (aphia_id in unique_aphia_ids) {
    # Fetch classification from WoRMS
    worms_classification <- fetch_classification_from_worms(aphia_id)
    #print(aphia_id)
    # Get the corresponding row from the input dataframe
    csv_row <- input_dataframe[input_dataframe$aphia_ID == aphia_id, ]
    
    # If the row does not exist, skip to the next AphiaID
    if (nrow(csv_row) == 0) {
      warning(paste("AphiaID", aphia_id, "not found in input dataframe."))
      next
    }
    
    # Compare each classification level and record mismatches
    mismatch <- list()
    for (level in names(worms_classification)) {
      if (level == "Species") next
      
      csv_value <- as.character(csv_row[[tolower(level)]])
      if(length(csv_value) > 1) {

        csv_value <- csv_value[1]
      }
      worms_value <- as.character(worms_classification[[level]])
      if(length(csv_value) > 1 || length(worms_value) > 1) next
      # Skip NA values
      if (is.na(csv_value) || (is.null(worms_value))) next
      
      if (length(csv_value) == 1 && length(worms_value) == 1 && tolower(csv_value) != tolower(worms_value)) {
        mismatch[[level]] <- list(CSV = csv_value, WoRMS = worms_value)
      }
    }
    
    if (length(mismatch) > 0) {
      mismatches[[as.character(aphia_id)]] <- mismatch
    }
  }
  print_mismatches(mismatches)
  return(mismatches)
}

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