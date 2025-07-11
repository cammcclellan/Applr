% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_slice.R
\docType{data}
\name{StatSlice}
\alias{StatSlice}
\title{StatSlice}
\format{
An object of class \code{ggproto}, inheriting from \code{Stat}
}
\usage{
StatSlice
}
\description{
A custom 'ggproto' object designed to take a high dimensional model,
pull the aesthetics from the global environment, and then create predictions
to display a slice of the high dimensional model.
}
\section{Parameters}{

\describe{
\item{model}{A fitted linear model object from 'lm()'}
\item{predict_vars}{A list of specified variables and values that specify the displayed slice}
\item{n}{The number of predicted points generated}
}
}

\section{Internal Methods}{

\describe{
\item{get_default_value}{An internal function that determines the mean of numeric columns or most common factor in fator/character columns and holds that value constant}
\item{compute_panel}{Generates prediction data with for each facet of a 'ggplot' graph}
}
}

\keyword{datasets}
