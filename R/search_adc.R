# Load necessary libraries
library(httr)
library(jsonlite)
library(xml2)
library(dplyr)
library(tidyr)
library(EDIutils)

# Function to query EDI for BLE data packages
query_edi <- function() {
  edi_packages <- search_data_packages("fq=scope:knb-lter-ble&q=*&fl=doi,packageid")
  # Extract the revision number
  edi_packages$revision <- as.numeric(sapply(strsplit(edi_packages$packageid, "\\."), tail, 1))
  # Reformat DOI column to be a URL
  edi_packages$doi <- gsub("doi:", "https://doi.org/", edi_packages$doi)
  return(edi_packages)
}

# Function to query ADC for BLE data packages
query_adc <- function(token) {
  adc_url <- "https://arcticdata.io/metacat/d1/mn/v2/query/solr/"
  response <- GET(
    url = adc_url,
    query = list(
      q = "scope:knb-lter-ble",
      fl = "identifier,title,revision",
      rows = 1000,
      wt = "xml"
    ),
    add_headers(Authorization = paste("Bearer", token))
  )
  if (status_code(response) == 200) {
    adc_packages <- fromJSON(content(response, as = "text"))$response$docs
    return(adc_packages)
  } else {
    stop("Failed to query ADC catalog. Check your token and network connection.")
  }
}

# Compare EDI and ADC packages and generate email text
compare_and_generate_email <- function(edi_packages, adc_packages) {
  # Convert ADC to a data frame and extract relevant fields
  adc_packages <- data.frame(
    seriesId = sapply(adc_packages$seriesId, function(x) ifelse(is.null(x), NA, x)),
    revision = as.numeric(adc_packages$revision),
    stringsAsFactors = FALSE
  )
  
  # Merge EDI and ADC data
  merged_data <- full_join(
    edi_packages %>% rename(edi_revision = revision),
    adc_packages %>% rename(adc_revision = revision),
    by = c("seriesId" = "seriesId")
  )
  
  # Identify missing or outdated packages
  missing_or_outdated <- merged_data %>%
    filter(is.na(adc_revision) | edi_revision > adc_revision)
  
  # Generate email text for missing or outdated packages
  email_text <- missing_or_outdated %>%
    mutate(
      email_line = ifelse(
        is.na(adc_revision),
        paste("The following package is missing in ADC:", doi),
        paste("The following package in ADC needs an update:", doi, "(current ADC revision:", adc_revision, "latest EDI revision:", edi_revision, ")")
      )
    ) %>%
    pull(email_line) %>%
    paste(collapse = "\n")
  
  return(email_text)
}

# Main script
token <- "eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJodHRwOlwvXC9vcmNpZC5vcmdcLzAwMDktMDAwMi04NDc1LTIyOTUiLCJmdWxsTmFtZSI6IkluZHVqYSBNb2hhbmRhcyIsImlzc3VlZEF0IjoiMjAyNC0xMi0wNVQxNzowODo1My44NjUrMDA6MDAiLCJjb25zdW1lcktleSI6InRoZWNvbnN1bWVya2V5IiwiZXhwIjoxNzMzNDgzMzMzLCJ1c2VySWQiOiJodHRwOlwvXC9vcmNpZC5vcmdcLzAwMDktMDAwMi04NDc1LTIyOTUiLCJ0dGwiOjY0ODAwLCJpYXQiOjE3MzM0MTg1MzN9.0CkJ_TY8greSBMBxy8kxB3iFRRMgQ35TZwFyyYQNy9rzh3gv9M98S42LZFuZ9q2fey1B4DWQ4K2Ti0Q2C6BCCSxG54GM_JpqWJGjDVIjwA4HpqwauM7M8RqSw6ItF2yw-a5AC956oBUJ9uG6jvTYhI-G5q4M96pfuMS2U5YFfPpaMWiFPjrxPJigCIvCJ2E5Y1ooIaFt63sO_yr_6CvGchgBk_ueHwtOINM7BQv0laad18zNcqMOGUCGwiXg3lXbyeOc1tB-jbFAdWyih7HgOtPUpOPRvstmSda3_OPHRCGqr7_zZQqBeg1tJyAXwWXw2P1SE_dcNyiiIbFyhYP1xA"
edi_packages <- query_edi()
adc_packages <- query_adc(token)

# Generate email text
email_text <- compare_and_generate_email(edi_packages, adc_packages)

# Save email text to a file
output_file <- "Data/utils/ADC_replication/email_text.txt"
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
writeLines(email_text, con = output_file)

cat("Email text generated and saved to:", output_file)