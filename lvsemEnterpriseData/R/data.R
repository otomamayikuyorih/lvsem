#' Enterprise PLS-SEM dataset metadata
#'
#' Metadata for the public enterprise and organizational datasets included in
#' this package.
#'
#' @format A data frame with one row per source dataset and columns describing
#'   source, license, unit of analysis, organization scope, and inclusion status.
#' @source See `source_url` and `doi` columns in the data.
"enterprise_metadata"

#' Enterprise and organizational datasets for latent variable SEM
#'
#' A named list of datasets. Each element contains:
#'
#' - `metadata`: one row from `enterprise_metadata`
#' - `raw_files`: files downloaded from the source repository
#' - `tables`: tabular files imported as data frames, with Excel workbooks stored
#'   as nested lists by sheet name
#'
#' Source datasets are distributed under CC BY 4.0 unless otherwise noted in
#' `enterprise_metadata` and `inst/NOTICE`.
#'
#' @format A named list.
#' @source See `enterprise_metadata`.
"enterprise_datasets"
