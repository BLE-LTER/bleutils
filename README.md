# bleutils

October 25th 2022

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
add_cp_cols(df, station_code_col = "station")
```

Note that this function relies on the `stations` data.frame packaged with `bleutils`. The canon copy of this is kept on Box. To check if `bleutils` has the latest version, use `update_cp_stations` and point it to where Box is on your local machine. If the Box version differs from what `bleutils` has, this will update the package version. If this happens, make sure to push the change to GitHub and re-install `bleutils` wherever it is used.

```r
update_cp_stations(source_file = "path_goes_here")
```

#### Inferring sampling season based on dates

`infer_season` outputs a vector of categorical season values (under ice/break up/open water) from a vector of date values (column in data.frame).

```r
df$season <- infer_season(df, date_col = "date_collected")
```

#### Order data columns

Order columns according to the standard BLE order. Check out the function documentation for the exact order depending on data type (water/sediment/mooring). Columns need to be named exactly as specified, so make sure that's done before running this function.

```r
order_cp_cols(df, type = "water")
```

#### Sort data rows

Order data rows according to the standard BLE order. Check out the function documentation for the exact order depending on data type (water/sediment/mooring). Columns need to be named exactly as specified, so make sure that's done before running this function.

```r
sort_cp_rows(df, type = "water")
```

### Other useful IM tasks

#### Initializing dataset

`init_datapkg` creates a directory structure and a templated R script for a new dataset. It will warn you if the dataset ID already exists (based on directory names in `base_path`) and messages the next available numeric ID.

```r
init_datapkg(base_path = getwd(),
             dataset_id = 9999,
             dataset_nickname = "test")
```

#### Initializing a data processing script

`init_script` creates a R script and populates it from template. For a new dataset, `init_datapkg` calls `init_script(type = "init")` and there's no further action needed R script. For updating existing datasets, use the argument `type = "update"`.

```r
init_script(dataset_id = 9999,
            workdir = file.path(getwd(), "9999_test", "EML_generation", "2022"), 
            type = "update")
```

The main difference is that an update script also has code to pull the latest version of the dataset from EDI's production server, so the new data can be appended to it.

The templates live in `inst/` if updates are needed.

#### Appending units to metadata attribute names

We decided to append abbreviated units at the end of attribute names (see BLE handbook). To facilitate that, wrap calls to `MetaEgress::get_meta()` in `bleutils::append_units`. This saves us from having to actually have the units in attribute names in metabase, and makes updates easier.

From this point example commands will not work without being able to connect to an instance of metabase. Feel free to run them if your machine can connect to an instance of metabase though.

```r
metadata <- MetaEgress::get_metadata(dbname = "ble_metabase",
                                       dataset_ids = 13, 
                                       user = "insert_or_enter_in_console", 
                                       password = "insert_or_enter_in_console")
metadata <- append_units(metadata)
```

#### Renaming attributes to match metadata

Quick convenience function to rename all columns in a data.frame to what's in the metadata. The usual assumptions apply: that there are as many columns in data as in metadata, and they are ordered the same way.

```r
df <- rename_attributes(metadata,
                          dataset_id = 13,
                          entity = 1,
                          x = df,
                          append_units = TRUE)
```

#### Exporting personnel from metabase to CSV

BLE's Core Program datasets include a CSV to personnel with the years of data they were involved in. This information is stored in metabase and is specific to BLE. `MetaEgress::get_meta()` doesn't query this information, so use:

```r
export_personnel(dataset_ids = 13,
                 file_name = "BLE_LTER_chlorophyll_personnel.csv")
```
