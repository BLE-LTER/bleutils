% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/init_script.R
\name{init_script}
\alias{init_script}
\title{Generate R processing script from template}
\usage{
init_script(dataset_id, file_dir = NULL, type = "update", data_year = NULL)
}
\arguments{
\item{dataset_id}{(numeric) Dataset ID that the generated script will process}

\item{file_dir}{(character) Directory to save script to. Defaults to the respective dataset's directory on An's machine if not specified.}

\item{type}{(character) either "init" or "update" for new or returning data packages respectively. Defaults to "update". The difference is in the contents of the script. Returning data packages get a script with a section where the data from the last revision on EDI's production is downloaded and used as the basis for appending new data onto.}

\item{data_year}{(numeric) The year of data that the generated script will process. E.g. you may be working with 2021 data in 2022. Defaults to the current calendar year if not specified.}
}
\description{
Generate R processing script from template
}
