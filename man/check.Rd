% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{check}
\alias{check}
\alias{test}
\alias{test.default}
\alias{expect}
\alias{expect.default}
\alias{assert}
\alias{assert.default}
\alias{need}
\alias{need.default}
\title{Validate S3 classes from pensieve}
\usage{
check(x)

test(x)

\method{test}{default}(x)

expect(x, info = NULL, label = NULL)

\method{expect}{default}(x, info = NULL, label = paste(class(x), "S3 class"))

assert(x, collection = NULL, var.name = NULL)

\method{assert}{default}(x, collection = NULL, var.name = paste(class(x)[1], "S3 class"))

need(x, label = NULL)

\method{need}{default}(x, label = NULL)
}
\arguments{
\item{x}{class object created by respective constructor function.}

\item{info}{[\code{character(1)}]\cr
See \code{\link[testthat]{expect_that}}}

\item{label}{[\code{character(1)}]\cr
See \code{\link[testthat]{expect_that}}}

\item{collection}{[\code{\link[checkmate]{AssertCollection}}]\cr
If an \code{\link[checkmate]{AssertCollection}} is provided, the error message is stored
in it. If \code{NULL}, an exception is raised if \code{res} is not
\code{TRUE}.}

\item{var.name}{[\code{character(1)}]\cr
The custom name for \code{x} as passed to any \code{assert*} function.
Defaults to a heuristic name lookup.}
}
\description{
Use \code{check()}, \code{test()}, \code{assert()}, \code{expect()} and \code{need()} to validate  classed objects from this package.

Check S3 class.
}
