#
# Processing script for an update to BLE LTER dataset ID datasetid
# This script is generated from a template housed under the R package bleutils
# with dataset-specific additions

# Setup

setwd(here::here())

library(EML)
library(MetaEgress)
library(bleutils)

# get metadata from metabase
# type in username and password in the R console
metadata <- append_units(get_meta(
  dbname = "ble_metabase",
  dataset_ids = datasetid,
  host = '10.157.18.83'
))

# Establish data folders
frompi <- file.path(getwd(), "..", "FromPI")
clean <- file.path(getwd(), "..", "Clean")

# read previous revision's data from EDI for authoritative source
# get newest revision number
rev <-
  max(as.integer(
    EDIutils::list_data_package_revisions("knb-lter-ble", datasetid)
  ))
pkg_id <-
  paste0("knb-lter-ble.", datasetid, ".", rev) # the revision number is left
# get entity names
entity_names <- EDIutils::read_data_entity_names(packageId = pkg_id)
# examine entity names here and decide which entity to get

# just one data entity
grab <- 1 # entity to get, change as needed
url <- EDIutils::read_data_package(pkg_id)[[grab]]
infile <- tempfile()
try(download.file(url, infile, method = "curl"))
prev_data <-
  data.table::fread(infile, colClasses = list(character = c("date_time", "date_collected"))) # use prev_data from now on

# multiple data entities example
# grab <- c(1, 2, 3) # entity or entities to get, change as needed
# url <- EDIutils::read_data_package(pkg_id)[grab]
# infiles <- list()
# prev <- list()
# for (i in seq_along(url)) {
#   infiles[[i]] <- tempfile()
#   try(download.file(url[[i]], infiles[[i]], method = "curl"))
#   prev[[i]] <- data.table::fread(infiles[[i]])
# }
# and then read in one-by-one

# Process data here


# EML generation

# get metadata from metabase
# type in username and password in the R console
metadata <- append_units(get_meta(
  dbname = "ble_metabase",
  dataset_ids = datasetid,
  host = '10.157.18.83'
))

# create entities in emld list structure
entities_datasetid <- create_entity_all(metadata,
                                        file_dir = clean,
                                        dataset_id = datasetid)

# create EML in emld list structure
# write to file only if resulting EML is schema valid
tryCatch({
  eml_datasetid <-
    create_EML(metadata, entities_datasetid, datasetid, here::here())
  if (eml_validate(eml_datasetid))
    # if above validation returns TRUE, then serialize to XML file
    write_eml(eml_datasetid,
              file = paste0("EML_", datasetid, "_", Sys.Date(), ".xml"))
},
error = function(e) {
  stop(e)
})
