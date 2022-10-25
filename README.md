# bleutils
October 25th 2022

## Orientation
`bleutils` is a collection of R functions for use by the BLE LTER IM team.

## Installation:

`remotes::install_github("BLE-LTER/bleutils")`

## Usage

### Manipulating Core Program data

First, load the example data. `cp_data` is included with `bleutils` and is an example of what investigators may give. 

```
library(bleutils)
df <- cp_data
```

#### Adding standard columns to CP data

`add_cp_cols` will add columns such as node, lagoon, lat/lon, etc... to a data.frame, provided a column of station codes, e.g. "EWLD1". Use this to quickly add the required columns to a dataset. 

```
add_cp_cols(df, station_code_col = "station")
```

Note that this function relies on the `stations` data.frame packaged with `bleutils`. The canon copy of this is kept on Box. To check if `bleutils` has the latest version, use `update_cp_stations` and point it to where Box is on your local machine. If the Box version differs from what `bleutils` has, this will update the package version. If this happens, make sure to push the change to GitHub and re-install `bleutils` wherever it is used.

```
update_cp_stations(source_file = "path_goes_here")
```

#### Inferring sampling season based on dates

`infer_season` outputs a vector of categorical season values (under ice/break up/open water) from a vector of date values (column in data.frame).

```
infer_season(df, date_col = "date_collected")
```

#### Order data columns

Order columns according to the standard BLE order. Check out the function documentation for the exact order depending on data type (water/sediment/mooring). Columns need to be named exactly as specified, so make sure that's done before running this function.

```
order_cp_cols(df, type = "water")
```

#### Sort data rows

Order data rows according to the standard BLE order. Check out the function documentation for the exact order depending on data type (water/sediment/mooring). Columns need to be named exactly as specified, so make sure that's done before running this function.

```
sort_cp_rows(df, type = "water")
```

### Other useful IM tasks

#### Initializing dataset

`init_datapkg` creates a folder structure and a templated R script for a new dataset. It will warn you if the dataset ID already exists (based on directory names in `base_path`) and messages the next available numeric ID.

```
init_datapkg(base_path = getwd(),
             dataset_id = 9999,
             dataset_nickname = "test")
```

#### Initializing a data processing script

`init_script` creates a R script from template. For a new dataset, `init_datapkg` calls `init_script` and there's no further action to initiate a R script. For updating existing datasets, use the argument `type = "update"` to 

```
init_script(dataset_id = 9999,
            workdir = file.path(getwd(), "9999_test", "EML_generation", "2022"), 
            type = "update")
```

#### Renaming attributes to match metadata

#### Exporting personnel from metabase to CSV
