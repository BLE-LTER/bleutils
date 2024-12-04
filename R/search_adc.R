#Load required libraries
library(XML)
library(httr)

# Set directory for saving output
output_dir <- "Data/utils/ADC_replication"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Set API endpoints and scope
adc_endpoint <- "https://arcticdata.io/metacat/d1/mn/v2/query/solr/"
edi_endpoint <- "https://pasta.lternet.edu/package/eml/"
edi_scope <- "knb-lter-ble" # Scope for BLE in EDI repository

# Function to search EDI packages
search_edi_packages <- function(scope) {
  url <- paste0(edi_endpoint, scope, "/")
  response <- GET(url)
  
  if (status_code(response) != 200) {
    stop("Failed to fetch data from EDI. Check your scope or network connection.")
  }
  
  edi_data <- content(response, as = "text", encoding = "UTF-8")
  package_ids <- unlist(strsplit(edi_data, "\n"))
  
  # Parse package identifiers into a data frame
  edi_df <- do.call(rbind, lapply(package_ids, function(pkg_id) {
    parts <- strsplit(pkg_id, "\\.")[[1]]
    data.frame(
      identifier = paste(parts[1:2], collapse = "."),
      revision = as.numeric(parts[3]),
      stringsAsFactors = FALSE
    )
  }))
  
  return(edi_df)
}

# Function to search ADC packages using XML
search_adc_packages <- function(query, token) {
  url <- paste0(adc_endpoint, "?q=", query, "&fl=identifier,revision&wt=xml")
  response <- GET(url, add_headers(Authorization = paste("Bearer", token)))
  
  if (status_code(response) != 200) {
    stop("Failed to fetch data from ADC. Check your token and query.")
  }
  
  xml_data <- content(response, as = "text", encoding = "UTF-8")
  adc_xml <- xmlParse(xml_data)
  
  # Extract identifiers and revisions
  identifiers <- xpathSApply(adc_xml, "//result/doc/str[@name='identifier']", xmlValue)
  revisions <- xpathSApply(adc_xml, "//result/doc/int[@name='revision']", xmlValue)
  
  adc_df <- data.frame(
    identifier = identifiers,
    revision = as.numeric(revisions),
    stringsAsFactors = FALSE
  )
  return(adc_df)
}

# Function to compare EDI and ADC packages
compare_packages <- function(edi_df, adc_df) {
  # Find missing packages
  missing_packages <- setdiff(edi_df$identifier, adc_df$identifier)
  
  # Check for outdated revisions
  common_packages <- intersect(edi_df$identifier, adc_df$identifier)
  edi_common <- edi_df[edi_df$identifier %in% common_packages, ]
  adc_common <- adc_df[adc_df$identifier %in% common_packages, ]
  
  outdated_packages <- edi_common[edi_common$revision > adc_common$revision, ]
  
  return(list(
    missing = missing_packages,
    outdated = outdated_packages
  ))
}

# Function to generate email text
generate_email_text <- function(missing, outdated) {
  email_text <- ""
  
  if (length(missing) > 0) {
    email_text <- paste0(
      email_text,
      "The following packages are missing from ADC and need harvesting from EDI:\n",
      paste(missing, collapse = "\n"), "\n\n"
    )
  }
  
  if (nrow(outdated) > 0) {
    email_text <- paste0(
      email_text,
      "The following packages have outdated metadata on ADC compared to EDI:\n",
      paste(outdated$identifier, collapse = "\n"), "\n\n"
    )
  }
  
  return(email_text)
}

# Main script
adc_query <- "abstract:BLE OR title:BLE OR keywords:BLE" # Adjust if needed
adc_token <- Sys.getenv("eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJodHRwOlwvXC9vcmNpZC5vcmdcLzAwMDktMDAwMi04NDc1LTIyOTUiLCJmdWxsTmFtZSI6IkluZHVqYSBNb2hhbmRhcyIsImlzc3VlZEF0IjoiMjAyNC0xMi0wNFQxOToyMzo1NC44MzUrMDA6MDAiLCJjb25zdW1lcktleSI6InRoZWNvbnN1bWVya2V5IiwiZXhwIjoxNzMzNDA1MDM0LCJ1c2VySWQiOiJodHRwOlwvXC9vcmNpZC5vcmdcLzAwMDktMDAwMi04NDc1LTIyOTUiLCJ0dGwiOjY0ODAwLCJpYXQiOjE3MzMzNDAyMzR9.pNXjSMVxsBdxpZbixkcqcC8XNxUvIgLzRfozN1OTsQAgt_pegq54Zhx3T9F_DeeFOu1C-gxhhxzCuyjpSsZ_HJ4elAmnRi2x-ciUdhxTjEfLf8CQvbROMWt3EqoPqmY9AckZ74K-LZLr_i6R2p6h4hCHlFBe102WZKFkDK90g5P53ZlvO6DNrB18uOfNdY6vrMxodFm9RFY1eLCDKcdT9xP6oTkAA15FU_Ga90CUw_jfGXVNWy45iR4LInC9HdeCPf02Bs4rO-xfdRFZ9ZZOoIWgbZViSx9x0nZCyt14LiszbfR6Z42Zb2Tb6fpc5aJvyIUc0D7HioWUe5uiIsnJhQ") # Store your ADC token in an environment variable

# Fetch EDI and ADC data
edi_df <- search_edi_packages(edi_scope)
adc_df <- search_adc_packages(adc_query, adc_token)

# Compare packages and generate email
comparison <- compare_packages(edi_df, adc_df)
email_text <- generate_email_text(comparison$missing, comparison$outdated)

# Save email text to file
email_file <- file.path(output_dir, "email_to_adc.txt")
writeLines(email_text, email_file)

cat("Comparison complete. Email text saved to:", email_file)