# bleutils

February 17th 2023

## Orientation

`bleutils` is a collection of R functions for use by the BLE LTER IM team.

## Installation

```r
remotes::install_github("BLE-LTER/bleutils")
```

## Usage

### Manipulating Core Program data

First, load the example data. `cp_data` is included with `bleutils` and is a minimal example of what investigators need to give the IM team before we can proceed.

```r
library(bleutils)
df <- cp_data
```

#### Adding standard columns to CP data

`add_cp_cols` will add columns such as node, lagoon, lat/lon, etc... to a data.frame, provided a column of station codes, e.g. "EWLD1". Use this to quickly add the required columns to a dataset.

```r
df <- add_cp_cols(df, station_code_col = "station")
```

Note that this function relies on a table of station information. This is [kept on Box](https://utexas.app.box.com/file/1092468994724?s=sjt5phkdpyx9vsvpvcss461562vb5wsw) and a direct download link to it is fed under the argument "station_source" to the function. To generate a direct link to the Box file, go to its Shared Link Settings, and copy the URL under "Direct Link". You can also feed the function a local file path instead of a Box link.

```r
update_cp_stations(source_file = "path_goes_here")
```

#### Inferring sampling season based on dates

`infer_season` outputs a vector of categorical season values (under ice/break up/open water) from a vector of date values (column in data.frame).
If the date vector is of type chr instead of Date or POSIXct (i.e., datetimes) as in the case of the example data, you will first need to convert the vector.

```r
# Example when dates are chr, without a time portion, e.g., "6/27/2019"
library(lubridate)
df$date_collected = as.Date(parse_date_time(df$date_collected, "mdy"))

# Now infer the season
df$season <- infer_season(df, date_col = "date_collected")
```

#### Order data columns

Order columns according to the standard BLE order. Check out the function documentation for the exact order depending on data type (water/sediment/mooring).
Columns need to be named exactly as specified, so make sure that's done before running this function.

```r
# In the example data, date_collected must be renamed to date_time
names(df)[names(df)=="date_collected"] <- "date_time"

# Now order the columns
df <- order_cp_cols(df, type = "water")
```

#### Sort data rows

Order data rows according to the standard BLE order. Check out the function documentation for the exact order depending on data type (water/sediment/mooring). Columns need to be named exactly as specified, so make sure that's done before running this function.

```r
df <- sort_cp_rows(df, type = "water")
```

### Other useful IM tasks

#### Initializing dataset

`init_datapkg` creates a directory structure and a templated R script for a new dataset. It will warn you if the dataset ID already exists (based on directory names in `base_path`) and messages the next available numeric ID.

```r
init_datapkg(base_path = getwd(),
             dataset_id = 9999L,
             dataset_nickname = "test")
```

#### Initializing a data processing script

`init_script` creates an R script and populates it from template. For a new dataset, `init_datapkg` calls `init_script(type = "init")` and there's no further action needed. For updating existing datasets, use the argument `type = "update"`.
Note that the file directory must exist before running this function.

```r
init_script(dataset_id = 9999,
            file_dir = file.path(getwd(), "9999_test", "EML_RProject_9999", "2022"), 
            type = "update")
```

The main difference is that an update script also has code to pull the latest version of the dataset from EDI's production server, so the new data can be appended to it.

The templates live in `inst/` if updates are needed.

#### Appending units to metadata attribute names

We decided to append abbreviated units at the end of attribute names (see BLE handbook). To facilitate that, wrap calls to `MetaEgress::get_meta()` in `bleutils::append_units`. This saves us from having to actually have the units in attribute names in metabase, and makes updates easier.

From this point example commands will not work without being able to connect to an instance of metabase. Feel free to run them if your machine can connect to an instance of metabase though.

```r
metadata <- MetaEgress::get_meta(dbname = "ble_metabase",
                                 dataset_ids = 13, 
                                 user = "insert_or_enter_in_console", 
                                 password = "insert_or_enter_in_console")
metadata <- append_units(metadata)
```

#### Renaming attributes to match metadata

`rename_attributes` renames all columns in a data.frame to what's in the metadata. The usual assumptions apply: that there are as many columns in data as in metadata, and they are ordered the same way.

```r
df <- rename_attributes(metadata,
                        dataset_id = 13,
                        entity = 1,
                        x = df,
                        append_units = TRUE)
```

#### Exporting personnel from metabase to CSV

BLE's Core Program datasets include a CSV of personnel with the years of data they were involved in. This information is stored in metabase and is specific to BLE. `MetaEgress::get_meta()` doesn't query this information, so use:

```r
export_personnel(dataset_ids = 13,
                 file_name = "BLE_LTER_chlorophyll_personnel.csv")
```

#### Fix incorrect stations during break-up season

During ice break-up, sometimes the field team cannot reach the actual station coordinates and end up at a nearby location. In 2023 for transparency we decided to label when this has happened with different station codes and the actual coordinates, instead of the station field team was supposed to be at. This function takes a CSV [kept on Box](https://utexas.app.box.com/file/1092468994724?s=sjt5phkdpyx9vsvpvcss461562vb5wsw) with the dates, the original stations affected, and new station codes to label these dates as. You can also use a local file with the same info. Note that this function strips the station information columns from the data.frame. They can be added back in via `add_cp_cols`, given that the Box station file also has the info for the new station codes. 

```r
df <- correct_stations(df)
```
