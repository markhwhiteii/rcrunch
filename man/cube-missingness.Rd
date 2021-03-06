% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cube-result.R
\name{cube-missingness}
\alias{cube-missingness}
\alias{showMissing}
\alias{hideMissing}
\alias{showIfAny}
\alias{showMissing,CrunchCube-method}
\alias{hideMissing,CrunchCube-method}
\alias{showIfAny,CrunchCube-method}
\title{Modify cube missing behavior}
\usage{
\S4method{showMissing}{CrunchCube}(cube)

\S4method{hideMissing}{CrunchCube}(cube)

\S4method{showIfAny}{CrunchCube}(cube)
}
\arguments{
\item{cube}{a CrunchCube}
}
\description{
By default, CrunchCubes do not show entries for missing categories. You can
include missing values in a \code{cube} with \code{showMissing(cube)} and hide them
again with \code{hideMissing(cube)}.
}
