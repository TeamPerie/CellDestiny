% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{MakeBiasPerTypeMatrix}
\alias{MakeBiasPerTypeMatrix}
\title{Make bias matrix}
\usage{
MakeBiasPerTypeMatrix(
  matrix,
  metadata,
  indivVar,
  indivVal,
  cellTypeVar,
  cellTypeVal,
  condition = "no",
  conditionVal = ""
)
}
\arguments{
\item{matrix, }{barcode count matrix}

\item{metadata, }{the metadata corresponding to the matrix}

\item{indivVar, }{name of variable defining individuals}

\item{indivVal, }{list of selected individuals}

\item{cellTypeVar, }{list of sected variables}

\item{cellTypeVal, }{list of sected values, at least 2.}

\item{condition, }{is there a condtion, default is "no"}

\item{conditionVal, }{condition value (only one accepted), default is ""}
}
\value{
a list of two matrices, the category for PlotCategories function and barcode count matrix for PlotCategoryCount function
}
\description{
Make bias matrix
}
