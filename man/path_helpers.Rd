% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_stringr.R
\name{path_helpers}
\alias{path_helpers}
\alias{list_files}
\alias{is_imagepath}
\title{List files in path}
\usage{
list_files(x, ...)

is_imagepath(x)
}
\arguments{
\item{x}{path}

\item{...}{other arguments passed to \link{list.files}}
}
\value{
list of files
}
\description{
A wrapper on top of \link{list.files} with \code{recursive} and \code{full.names} set to \code{TRUE}
}
\section{Functions}{
\itemize{
\item \code{is_imagepath}: test if a path has an image extension
}}

\examples{
Momit_data("acer") \%>\% list.files()
Momit_data("acer") \%>\% list_files()
}
