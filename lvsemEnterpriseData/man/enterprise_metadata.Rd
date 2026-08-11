\name{enterprise_metadata}
\alias{enterprise_metadata}
\title{Enterprise PLS-SEM Dataset Metadata}
\description{
Metadata for public enterprise and organizational datasets included in
\pkg{lvsemEnterpriseData}.
}
\format{
A data frame with one row per source dataset. Important columns include:
\describe{
  \item{no}{Reference number from the source inventory.}
  \item{dataset_id}{Package-safe dataset identifier.}
  \item{title}{Source dataset title.}
  \item{repository}{Source repository, such as Mendeley Data or Figshare.}
  \item{repository_id}{Repository dataset or article identifier.}
  \item{source_url}{Original source URL.}
  \item{license}{Dataset license.}
  \item{license_url}{Dataset license URL.}
  \item{enterprise_data_degree}{Curated rating of closeness to enterprise or
  organizational internal data.}
  \item{included}{Whether the dataset has imported tabular data and is included
  as immediately usable package data.}
  \item{raw_file_count}{Number of downloaded source files.}
  \item{table_count}{Number of tabular files imported into R.}
  \item{has_tabular_data}{Whether at least one table was imported.}
}
}
\source{
Public data repositories listed in the \code{source_url} column.
}
\keyword{datasets}
