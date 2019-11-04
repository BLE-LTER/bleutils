#
# Processing script for BLE LTER dataset ID datasetid
# This script is generated from a templated housed under the R package bleutils
# With dataset-specific additions

# Setup

setwd(here::here())

library(EML)
library(MetaEgress)
library(bleutils)

# Read in data from folder



# Process data here


# EML generation
# execute this block and you'll get an EML file! at least that's the hope


metadata <- get_meta("ble_metabase", dataset_ids = datasetid)
entities_datasetid <- create_entity_all(metadata, here::here(), datasetid)

tryCatch({
  eml_datasetid <- create_EML(metadata, entities_datasetid, datasetid, here::here())
  if(eml_validate(eml_datasetid)) # if above validation returns TRUE, then serialize to XML file
    write_eml(eml_datasetid, file = paste0("EML_", datasetid, "_", Sys.Date(), ".xml"))
},
error = function(e) {
  error(e)
})


