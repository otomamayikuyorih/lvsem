\name{pls_ema}
\alias{pls_ema}
\alias{print.pls_ema}
\title{Partial Least Squares Exploratory Mediation Analysis}
\usage{
pls_ema(
  data,
  x,
  mediators,
  y,
  br = 1000,
  seed = 123,
  conf.level = 0.95
)

\method{print}{pls_ema}(x, ...)
}
\arguments{
  \item{data}{Data frame containing all analysis variables.}
  \item{x}{Character vector naming exposure-block variables.}
  \item{mediators}{Character vector naming candidate mediator variables.}
  \item{y}{Character vector naming outcome-block variables.}
  \item{br}{Number of case-bootstrap samples; zero omits bootstrapping.}
  \item{seed}{Random seed.}
  \item{conf.level}{Percentile-bootstrap confidence level.}
  \item{...}{Additional arguments passed to \code{print}.}
}
\value{
An object of class \code{pls_ema} containing objective covariances, standardized
score-path effects, mediator weights and loadings, scores, the multiset PLS fit,
and bootstrap draws.
}
\description{
Extracts an exploratory mediator component from three variable blocks using
the fixed objective
\deqn{cov(Xw, Mc) + cov(Mc, Yu) - cov(Xw, Yu).}
Each case-bootstrap sample repeats component and path estimation.
}
\details{
PLS-EMA is exploratory. The reported path products are not identified causal
mediation effects without an appropriate design and assumptions.
}
