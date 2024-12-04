# Load necessary libraries
library(httr)
library(xml2)
library(dplyr)
library(EDIutils)

# Define function to fetch all package IDs dynamically from EDI using search_data_packages
fetch_all_edi_package_ids <- function(query) {
  # Use EDIutils function to search EDI for packages based on query
  #edi_packages <- search_data_packages(query)  # Updated to use search_data_packages() function
  
  edi_packages <- search_data_packages("q=keyword:BLE&fl=id")
  print(edi_packages)
  if (nrow(edi_packages) == 0) {
    stop("No valid package IDs retrieved from EDI.")
  }
  
  # Extract package IDs from the returned data
  ids <- edi_packages$packageId
  
  return(ids)
}

# Define function to fetch metadata for a specific package ID from ADC using rsolr
fetch_adc_metadata <- function(package_id, token) {
  adc_base_url <- "https://arcticdata.io/metacat/d1/mn/v2/query/solr"
  
  # Extract scope and identifier from package ID (ensure correct format)
  components <- unlist(strsplit(package_id, "\\."))
  if (length(components) < 2) {
    stop("Invalid package ID format. Expected format: <scope>.<identifier>.<revision>")
  }
  query_id <- paste(components[1:2], collapse = ".")
  
  # Construct query URL for ADC
  query_url <- paste0(adc_base_url, "?q=id:", query_id, "&fl=revision")
  
  # Fetch metadata from ADC
  response <- GET(query_url, add_headers(Authorization = paste("Bearer", token)))
  
  if (response$status_code != 200) {
    stop("Failed to query ADC for package ID: ", package_id)
  }
  
  # Parse XML response and extract revision
  xml_content <- content(response, "text", encoding = "UTF-8")
  xml_parsed <- read_xml(xml_content)
  revisions <- xml_find_first(xml_parsed, ".//revision") %>% xml_text() %>% as.numeric()
  
  if (is.na(revisions)) {
    message("No metadata found in ADC for package ID: ", package_id)
    return(NA)
  }
  
  return(revisions)
}

# Function to compare revisions between EDI and ADC
compare_revisions <- function(package_id, token) {
  # Fetch EDI revision using EDIutils function
  edi_metadata <- edi_get_metadata(package_id)
  edi_revision <- as.numeric(edi_metadata$revision)
  
  # Fetch ADC revision using custom function
  adc_revision <- fetch_adc_metadata(package_id, token)
  
  # Compare revisions and generate result
  if (is.na(adc_revision)) {
    result <- paste("Package ID:", package_id, "| Status: Missing in ADC | EDI Link: https://pasta.lternet.edu/package/metadata/eml/", package_id)
  } else if (adc_revision < edi_revision) {
    result <- paste("Package ID:", package_id, "| EDI Revision:", edi_revision, "| ADC Revision:", adc_revision, "| Status: Outdated in ADC | EDI Link: https://pasta.lternet.edu/package/metadata/eml/", package_id)
  } else {
    result <- paste("Package ID:", package_id, "| EDI Revision:", edi_revision, "| ADC Revision:", adc_revision, "| Status: Up-to-date")
  }
  
  return(result)
}

# Main script
# Define token and EDI query
adc_token <-"eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJodHRwOlwvXC9vcmNpZC5vcmdcLzAwMDktMDAwMi04NDc1LTIyOTUiLCJmdWxsTmFtZSI6IkluZHVqYSBNb2hhbmRhcyIsImlzc3VlZEF0IjoiMjAyNC0xMi0wNFQxOToyMzo1NC44MzUrMDA6MDAiLCJjb25zdW1lcktleSI6InRoZWNvbnN1bWVya2V5IiwiZXhwIjoxNzMzNDA1MDM0LCJ1c2VySWQiOiJodHRwOlwvXC9vcmNpZC5vcmdcLzAwMDktMDAwMi04NDc1LTIyOTUiLCJ0dGwiOjY0ODAwLCJpYXQiOjE3MzMzNDAyMzR9.pNXjSMVxsBdxpZbixkcqcC8XNxUvIgLzRfozN1OTsQAgt_pegq54Zhx3T9F_DeeFOu1C-gxhhxzCuyjpSsZ_HJ4elAmnRi2x-ciUdhxTjEfLf8CQvbROMWt3EqoPqmY9AckZ74K-LZLr_i6R2p6h4hCHlFBe102WZKFkDK90g5P53ZlvO6DNrB18uOfNdY6vrMxodFm9RFY1eLCDKcdT9xP6oTkAA15FU_Ga90CUw_jfGXVNWy45iR4LInC9HdeCPf02Bs4rO-xfdRFZ9ZZOoIWgbZViSx9x0nZCyt14LiszbfR6Z42Zb2Tb6fpc5aJvyIUc0D7HioWUe5uiIsnJhQ"
edi_query <- 'q=keyword:BLE&fl=id'

# Fetch all package IDs dynamically from EDI using EDIutils
package_ids <- fetch_all_edi_package_ids(edi_query)$id
print(package_ids)

# Compare revisions for each package ID
results <- sapply(package_ids, compare_revisions, token = adc_token)
print(results)

# Filter and prepare results that need harvesting from ADC
harvest_needed <- results[grep("Status: Missing in ADC|Status: Outdated in ADC", results)]

# Create email text with links to EDI packages needing harvesting
email_text <- paste("Dear ADC team,\n\nThe following EDI packages are missing or outdated in the ADC catalog and require harvesting:\n\n", 
                    paste(harvest_needed, collapse = "\n"), 
                    "\n\nPlease let us know once the harvesting is complete. Thank you.\n\nBest regards,\nInduja")

# Save the results to a text file
writeLines(email_text, "C:/Users/im23237/Documents/email_text.txt")

# Print the email text
cat(email_text)