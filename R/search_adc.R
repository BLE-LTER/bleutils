# Load necessary libraries
library(dataone)
library(EDIutils)
library(dplyr)

query_edi <- function() {
  edi_packages <- search_data_packages("fq=scope:knb-lter-ble&q=*&fl=doi,packageid")
  # Extract the revision number
  edi_packages$revision <- as.numeric(sapply(strsplit(edi_packages$packageid, "\\."), tail, 1))
  # Reformat DOI column to be a URL
  edi_packages$doi <- gsub("doi:", "https://doi.org/", edi_packages$doi)
  return(edi_packages)
}

# Set your DataONE authentication token
options(options(dataone_token = "eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJodHRwOlwvXC9vcmNpZC5vcmdcLzAwMDktMDAwMi04NDc1LTIyOTUiLCJmdWxsTmFtZSI6IkluZHVqYSBNb2hhbmRhcyIsImlzc3VlZEF0IjoiMjAyNC0xMi0wNlQwNToxMjo1MS4xODErMDA6MDAiLCJjb25zdW1lcktleSI6InRoZWNvbnN1bWVya2V5IiwiZXhwIjoxNzMzNTI2NzcxLCJ1c2VySWQiOiJodHRwOlwvXC9vcmNpZC5vcmdcLzAwMDktMDAwMi04NDc1LTIyOTUiLCJ0dGwiOjY0ODAwLCJpYXQiOjE3MzM0NjE5NzF9.V_bVSf6DsNn64eMsZtUkTd73ST4ipWZ9U55GErYLwu2bKeWQVhjOtw7AHy2HrCxdPFo1Nen6ocmVcBbx78-ojnQsrosW62o4uK9mMW1CvQtEVe65ESBwcCPMihI2D_8cP0YbDIV4cSbQIVsPsXbY-GM2bi1heS7nBQUvtyqXcM3PqF2k1T73BsTA5Dm95Hi-oQv_SkfwWoQ2i2h1gs-s8f4YFhaz8AFc_sl-YxAK-QvxaakPM1ZRuM6Uv9anrG8yjrPLahwBwPfDpD_BYxsqSK6hHyFUWRabmXxzxUPBlVddsAto4m6BrVuc3UfThlasAcLXvrRJ9COkkqyhk4WpJA")) # Replace 'your_token_here' with your actual token

# Initialize the DataONE client for the Arctic Data Center
cn <- CNode("PROD")                         # Connect to the production DataONE environment
mn <- getMNode(cn, "urn:node:ARCTIC")       # Specify the Arctic Data Center member node

# Define the Solr query parameters
query <- list(
  q = 'id:*//pasta.lternet.edu/package/metadata/eml/knb-lter-ble/*', # Solr query string
  rows = 100,                                                        # Number of results to return
  fl = "id"                                                          # Fields to retrieve
)

# Query EDI and ADC for packages
edi_packages <- query_edi()
result <- dataone::query(mn, solrQuery = query, as = "data.frame")

# Check if results are returned
if (nrow(result) > 0) {
  # Extract dataset IDs and revisions from the retrieved URLs
  dataset_data <- do.call(rbind, strsplit(result$id, "/"))[, 8:9]
  dataset_data <- as.data.frame(dataset_data, stringsAsFactors = FALSE)
  colnames(dataset_data) <- c("dataset_id", "revision")
  
  # Convert revision to numeric for filtering
  dataset_data$revision <- as.numeric(dataset_data$revision)
  
  # Filter to keep only the latest revision for each dataset ID
  latest_revisions <- dataset_data %>%
    group_by(dataset_id) %>%
    slice_max(order_by = revision, n = 1) %>%
    ungroup()
  
  # Add package ID column
  latest_revisions$packageid <- paste0("knb-lter-ble.", latest_revisions$dataset_id, ".", latest_revisions$revision)
  
  # Compare and identify missing or outdated package IDs
  missing_packages <- setdiff(edi_packages$packageid, latest_revisions$packageid)
  
  if (length(missing_packages) > 0) {
    # Reconstruct DOI links for missing packages
    missing_packages_links <- edi_packages[edi_packages$packageid %in% missing_packages, ]
    
    # Generate email text
    email_text <- "Dear Arctic Data Center,\n\n"
    email_text <- paste0(email_text, "Please find the details of the EDI packages that need to be harvested to reflect their latest metadata revisions on ADC:\n\n")
    
    for (i in 1:nrow(missing_packages_links)) {
      email_text <- paste0(email_text, "- Package ID: ", missing_packages_links$packageid[i], "\n",
                           "  Link: ", missing_packages_links$doi[i], "\n\n")
    }
    
    email_text <- paste0(email_text, "Thank you for your attention.\n\nBest regards,\n[Your Name]")
    
    # Save email text to a file
    save_path <- "Data/utils/ADC_replication/email_text.txt"
    dir.create("Data/utils/ADC_replication", showWarnings = FALSE, recursive = TRUE)
    writeLines(email_text, con = save_path)
    cat("Output saved to:", save_path, "\n")
  } else {
    # Save an "up-to-date" message to the file
    email_text <- "All package IDs are up-to-date in the Arctic Data Center.\n"
    save_path <- "Data/utils/ADC_replication/email_text.txt"
    dir.create("Data/utils/ADC_replication", showWarnings = FALSE, recursive = TRUE)
    writeLines(email_text, con = save_path)
    cat("Output saved to:", save_path, "\n")
  }
} else {
  # Save a "no datasets found" message to the file
  email_text <- "No datasets found in the query.\n"
  save_path <- "Data/utils/ADC_replication/email_text.txt"
  dir.create("Data/utils/ADC_replication", showWarnings = FALSE, recursive = TRUE)
  writeLines(email_text, con = save_path)
  cat("Output saved to:", save_path, "\n")
}