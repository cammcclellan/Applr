% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatter_3d.R
\name{scatter_3d}
\alias{scatter_3d}
\title{Plot a 3D-grapheable linear model}
\usage{
scatter_3d(model, n = 100, colors = c("blue", "yellow"))
}
\arguments{
\item{model}{A saved linear model with 2 predictor variables}

\item{n}{Number of evaluations}

\item{colors}{Colors used for the gradient scale}
}
\value{
A 3D graph in the viewer section
}
\description{
Plot a 3D-grapheable linear model
}
\examples{
\dontrun{
model <- lm(mpg ~ disp + hp, data = mtcars)
scatter_3d(model, colors=c('blue','yellow'))
}

}
