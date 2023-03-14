#
# Processing script for the first upload of BLE LTER dataset ID datasetid
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
metadata <- append_units(get_meta(dbname = "ble_metabase",
                     dataset_ids = datasetid,
                     host = '10.157.18.83'))

# Read in data from folder
frompi <- file.path(getwd(), "..", "FromPI")
# get latest subfolder in frompi
y <- list.dirs(frompi)[[length(list.dirs(frompi))]]
# frompi is set to latest subfolder in frompi
frompi <- file.path(frompi, basename(y))
clean <- file.path(getwd(), "..", "Clean")

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
    write_eml(eml_datasetid, file = file.path(datayear, paste0("EML_", datasetid, "_", Sys.Date(), ".xml")))
},
error = function(e) {
  stop(e)
})



