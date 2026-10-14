#' @title Compare ADC to EDI
#' @description Reports datasets at EDI that the Arctic Data Center needs to harvest
#' @param edi_auth_token (character) The EDI API authentication key
#' @param adc_auth_token (character) The authentication token for the Arctic Data Center. See https://arcticdata.io/catalog/api
#' @export

compare_adc_to_edi <- function(edi_auth_token, adc_auth_token) {
  library(dataone)
  library(EDIutils)
  library(dplyr)
  
  # Ask for API keys
  edi_auth_token <- rstudioapi::askForPassword("Enter EDI API key")
  adc_auth_token <- rstudioapi::askForPassword("Enter ADC API token")
  
  # Authenticate with EDI API
  EDIutils::login(key = edi_auth_token)
  
  # Query EDI
  edi_packages <- search_data_packages("fq=scope:knb-lter-ble&q=*&fl=doi,packageid")
  edi_packages$dataset <- as.numeric(sapply(strsplit(edi_packages$packageid, "\\."), function(x) x[2]))
  edi_packages$revision <- as.numeric(sapply(strsplit(edi_packages$packageid, "\\."), tail, 1))
  edi_packages$doi <- gsub("doi:", "https://doi.org/", edi_packages$doi)
  
  # Query ADC
  options(options(dataone_token = adc_auth_token))  # Set authentication token
  
  httr::set_config(httr::user_agent("Mozilla/5.0"))
  httr::set_config(httr::config(http_version = 2L))
  cn <- CNode("PROD")  # Connect to the production DataONE environment
  mn <- getMNode(cn, "urn:node:ARCTIC")  # Specify the Arctic Data Center member node
  query <- list(
    q = 'id:*//pasta.lternet.edu/package/metadata/eml/knb-lter-ble/*',
    rows = 100,  # Number of results to return. TODO: Increase this number if necessary
    fl = "id"  # Fields to retrieve
  )
  adc_packages <- dataone::query(mn, solrQuery = query, as = "data.frame")
  # Extract dataset IDs and revisions from the retrieved URLs
  dataset_data <- do.call(rbind, strsplit(adc_packages$id, "/"))[, 8:9]
  dataset_data <- as.data.frame(dataset_data, stringsAsFactors = FALSE)
  colnames(dataset_data) <- c("dataset_id", "revision")
  # Convert revision to numeric for filtering
  dataset_data$revision <- as.numeric(dataset_data$revision)
  # Filter to keep only the latest revision for each dataset ID
  latest_revisions <- dataset_data %>%
    group_by(dataset_id) %>%
    slice_max(order_by = revision, n = 1) %>%
    ungroup()
  latest_revisions$packageid <- paste0("knb-lter-ble.", latest_revisions$dataset_id, ".", latest_revisions$revision)
  
  # Compare and identify missing or outdated package IDs
  missing_package_ids <- setdiff(edi_packages$packageid, latest_revisions$packageid)
  
  if (length(missing_package_ids) > 0) {
    # Reconstruct DOI links for missing packages
    missing_packages <- edi_packages[edi_packages$packageid %in% missing_package_ids, ]
    # sort missing_packages by the dataset column
    missing_packages <- missing_packages[order(missing_packages$dataset), ]
    
    # Generate email text
    email_text <- "Dear Arctic Data Center,\n\n"
    email_text <- paste0(email_text, "Will you please harvest metadata for the new/revised EDI packages below for BLE LTER?  Thanks!\n\n")
    
    for (i in 1:nrow(missing_packages)) {
      email_text <- paste0(email_text, "Package ID: ", missing_packages$packageid[i], "\n",
                           missing_packages$doi[i], "\n\n")
    }
    
    # Print email text to console
    cat(email_text)
  } else {
    cat("ADC and EDI are in sync")
  }
}