\name{lvsem_core}
\alias{make_lv_scores}
\alias{estimate_paths}
\alias{bootstrap_paths}
\alias{effects_from_beta}
\alias{measurement_summary}
\title{Core lvsem Estimation Helpers}
\usage{
make_lv_scores(dat, blocks, path_matrix)

estimate_paths(score_pls, path_matrix, scale_scores = TRUE)

bootstrap_paths(
  dat,
  blocks,
  path_matrix,
  br = 1000,
  seed = 123
)

effects_from_beta(beta_df)

measurement_summary(dat, blocks, scores)
}
\arguments{
  \item{dat}{Data frame containing manifest variables.}
  \item{blocks}{Named list mapping constructs to manifest-variable names.}
  \item{path_matrix}{Square structural path matrix in \code{[to, from]} form.}
  \item{score_pls, scores}{Data frame of latent-variable scores.}
  \item{scale_scores}{Whether to standardize scores before path estimation.}
  \item{br}{Number of case-bootstrap samples.}
  \item{seed}{Random seed.}
  \item{beta_df}{Data frame with \code{path} and \code{beta} columns.}
}
\value{
Scores, path tables, effect decompositions, or measurement summaries,
depending on the function called.
}
\description{
Helpers for multiset PLS construct scores, structural paths, bootstrap
uncertainty, effect decomposition, and reflective measurement summaries.
}
