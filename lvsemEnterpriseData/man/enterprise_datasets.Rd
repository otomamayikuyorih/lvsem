\name{enterprise_datasets}
\alias{enterprise_datasets}
\title{Enterprise and Organizational Datasets for Latent Variable SEM}
\description{
A named list of enterprise and organizational datasets for PLS-SEM examples.
}
\format{
A named list. Each element contains:
\describe{
  \item{metadata}{One row from \code{\link{enterprise_metadata}}.}
  \item{raw_files}{Relative paths to downloaded source files under
  \code{inst/extdata/raw}.}
  \item{tables}{Imported tabular data. CSV and TSV files are data frames;
  Excel workbooks are represented as named lists of sheets.}
}
}
\details{
Source datasets are distributed under CC BY 4.0 unless otherwise noted in
\code{\link{enterprise_metadata}} and \code{inst/NOTICE}. Files that could not
be imported as tabular data are still preserved as raw source files when
available.
}
\source{
Public data repositories listed in \code{\link{enterprise_metadata}}.
}
\keyword{datasets}
