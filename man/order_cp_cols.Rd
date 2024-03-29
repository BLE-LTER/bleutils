% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/order_cp_cols.R
\name{order_cp_cols}
\alias{order_cp_cols}
\title{Order Core Program data columns}
\usage{
order_cp_cols(data, type)
}
\arguments{
\item{data}{(data.frame) A data.frame containing Core Program data. Columns names need to be named according to standard CP column names. Use \code{bleutils::rename_attributes} prior to running this to rename columns to what's in metadata.}

\item{type}{(character) Type of CP data. Choose from "water", "sediment", or "mooring". Use "sediment" for biota data as well.}
}
\description{
Order Core Program data columns in this particular order: node, lagoon, station, season, date_time, water_column_position, (insert data columns), station_name, latitude, longitude, station_depth, habitat_type, station_sampling_priority. Note that station_depth is optional.
}
