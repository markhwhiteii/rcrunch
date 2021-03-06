% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decks.R
\name{decks}
\alias{decks}
\alias{decks<-}
\alias{decks,CrunchDataset-method}
\title{Get a dataset's DeckCatalog}
\usage{
decks(x)

decks(x) <- value

\S4method{decks}{CrunchDataset}(x)
}
\arguments{
\item{x}{a Crunch Dataset}

\item{value}{a \code{CrunchDeck} to add}
}
\value{
a DeckCatalog
}
\description{
Crunch decks are stored in catalogs. This function returns those catalogs so
that you can access and manipulate decks in R.
}
