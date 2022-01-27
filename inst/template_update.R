#
# Processing script for an update to BLE LTER dataset ID datasetid
# This script is generated from a templated housed under the R package bleutils
# With dataset-specific additions

# Setup

setwd(here::here())

library(EML)
library(MetaEgress)
library(bleutils)

# get metadata from metabase
# type in username and password in the R console
metadata <- append_units(get_meta(dbname = "ble_metabase",
                                  dataset_ids = datasetid,
                                  host = '10.157.18.83'))

# Establish data folders
frompi <- file.path(getwd(), "..", "FromPI")
clean <- file.path(getwd(), "..", "Clean")

# read previous revision's data from EDI for authoritative source
# get newest revision number
rev <- max(as.integer(EDIutils::api_list_data_package_revisions("knb-lter-ble", datasetid)))
pkg_id <- paste0("knb-lter-ble.", datasetid, ".", rev) # the revision number is left
# get entity names
entity_ids <- EDIutils::api_list_data_entities(pkg_id)[["identifier"]]
entity_names <- sapply(entity_ids, EDIutils::api_read_data_entity_name, package.id = pkg_id)

# decide which entity to get
grab <- 1 # change as needed
url <- EDIutils::api_read_data_package(pkg_id)[[grab]]
infile <- tempfile()
try(download.file(url, infile, method="curl"))
prev_data <- data.table::fread(infile) # use prev_data from now on


# Process data here


# EML generation

# get metadata from metabase
# type in username and password in the R console
metadata <- append_units(get_meta(dbname = "ble_metabase",
                                  dataset_ids = datasetid,
                                  host = '10.157.18.83'))

# create entities in emld list structure
entities_datasetid <- create_entity_all(metadata,
                                        file_dir = clean,
                                        dataset_id = datasetid)

# create EML in emld list structure
# write to file only if resulting EML is schema valid
tryCatch({
  eml_datasetid <- create_EML(metadata, entities_datasetid, datasetid, here::here())
  if(eml_validate(eml_datasetid)) # if above validation returns TRUE, then serialize to XML file
    write_eml(eml_datasetid, file = paste0("EML_", datasetid, "_", Sys.Date(), ".xml"))
},
error = function(e) {
  stop(e)
})
