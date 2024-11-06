#
# Processing script for an update to BLE LTER dataset ID datasetid
# This script is generated from a template housed under the R package bleutils
# with dataset-specific additions

# Setup

setwd(here::here())

library(EML)
library(MetaEgress)
library(bleutils)
library(dplyr)

# get metadata from metabase
# type in username and password in the R console
metadata <- append_units(get_meta(
  dbname = "ble_metabase",
  dataset_ids = datasetid,
  host = '10.157.18.158'
))

# Establish data folders
frompi <- file.path(getwd(), "..", "FromPI")
# get latest subfolder in frompi
y <- list.dirs(frompi)[[length(list.dirs(frompi))]]
# frompi is set to latest subfolder in frompi
frompi <- file.path(frompi, basename(y))
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
  data.table::fread(infile,
                    colClasses = list(character = c("date_time"))) # make sure date times are read in as character as to not mess up our formatting

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
  host = '10.157.18.158'
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
              file = file.path(as.character(datayear), paste0("EML_", datasetid, "_", Sys.Date(), ".xml")))
},
error = function(e) {
  stop(e)
})

# insert additionalMetadata snippet for replication to ADC
# to the EML document we just created
insert_additional_metadata(file = file.path(as.character(datayear), paste0("EML_", datasetid, "_", Sys.Date(), ".xml")))
