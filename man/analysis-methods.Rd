% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slides.R
\name{analyses}
\alias{analyses}
\alias{analysis}
\alias{analysis<-}
\alias{query<-}
\alias{cube}
\alias{cubes}
\alias{filter}
\alias{filter<-}
\alias{analyses,CrunchSlide-method}
\alias{analysis,CrunchSlide-method}
\alias{analysis<-,CrunchSlide,formula-method}
\alias{analysis<-,CrunchSlide,Analysis-method}
\alias{filter,CrunchSlide-method}
\alias{filter<-,CrunchSlide,ANY-method}
\alias{query<-,CrunchSlide,ANY-method}
\alias{cubes,CrunchSlide-method}
\alias{cube,CrunchSlide-method}
\alias{cubes,AnalysisCatalog-method}
\alias{query<-,Analysis,formula-method}
\alias{cube,Analysis-method}
\alias{filter,Analysis-method}
\alias{filter,ANY-method}
\alias{filter<-,Analysis,CrunchLogicalExpr-method}
\alias{filter<-,Analysis,CrunchFilter-method}
\alias{filter<-,Analysis,NULL-method}
\title{Get and set slide analyses}
\usage{
analyses(x)

analysis(x)

analysis(x) <- value

query(x) <- value

cube(x)

cubes(x)

filter(x, ...)

filter(x) <- value

\S4method{analyses}{CrunchSlide}(x)

\S4method{analysis}{CrunchSlide}(x)

\S4method{analysis}{CrunchSlide,formula}(x) <- value

\S4method{analysis}{CrunchSlide,Analysis}(x) <- value

\S4method{filter}{CrunchSlide}(x, ...)

\S4method{filter}{CrunchSlide,ANY}(x) <- value

\S4method{query}{CrunchSlide,ANY}(x) <- value

\S4method{cubes}{CrunchSlide}(x)

\S4method{cube}{CrunchSlide}(x)

\S4method{cubes}{AnalysisCatalog}(x)

\S4method{query}{Analysis,formula}(x) <- value

\S4method{cube}{Analysis}(x)

\S4method{filter}{Analysis}(x, ...)

\S4method{filter}{ANY}(x, ...)

\S4method{filter}{CrunchSlide,ANY}(x) <- value

\S4method{filter}{Analysis,CrunchLogicalExpr}(x) <- value

\S4method{filter}{Analysis,CrunchFilter}(x) <- value

\S4method{filter}{Analysis,`NULL`}(x) <- value
}
\arguments{
\item{x}{a \code{CrunchSlide}, \code{AnalysisCatalog}, or \code{Analysis}}

\item{value}{for the setter, a query}

\item{...}{ignored}
}
\value{
an \code{AnalysisCatalog}, \code{Analysis}, \code{Cube}, or \code{Filter}
}
\description{
Slides are composed of analyses, which are effectively \code{CrunchCubes} with some
additional metadata. You can get and set a slide's Analysis Catalog with the
\code{analyses} method, and access an individual analysis with \code{analysis}.
}
\details{
You can get the \code{CrunchCube} from a slide or analysis with the \code{cube} method and
from a \code{CrunchDeck} with \code{cubes}. Analyses can be changed by assigning a formula
into the \code{query} function.
}
\examples{
\dontrun{
analysis(slide)
cube(slide)
cubes(deck)
query(slide) <- ~ cyl + wt
filter(slide)
filter(slide) <- NULL # to remove a filter
filter(slide) <- filters(ds)[["My filter"]]
}
}
