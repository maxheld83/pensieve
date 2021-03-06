% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/markup2vector.R
\name{render_chain}
\alias{render_chain}
\title{Render a list of markdown vectors to a desired format.}
\usage{
render_chain(l, format, ...)
}
\arguments{
\item{l}{\verb{[list()]}
giving \code{x}s to be passed to \code{\link[=md2tex_mem]{md2tex_mem()}}.}

\item{format}{\verb{[character(1)]} giving the output format to render items in.

Must be one of:
\itemize{
\item \code{'tex'}
\item \code{'pdf'} (\strong{default})
\item \code{'svg'} or
\item \code{'grob'}
}}

\item{...}{arguments passed on to downstream formatting functions}
}
\value{
\verb{[list()]}
of output format objects.
}
\description{
Goes through the conversion chain as long as necessary to return the desired output.
}
\keyword{internal}
