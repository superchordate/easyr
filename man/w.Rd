% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/w.R
\name{w}
\alias{w}
\title{Write}
\usage{
w(x, filename = "out", row.names = FALSE, na = "")
}
\arguments{
\item{x}{Data frame to write to file.}

\item{filename}{(Optional) Filename to use.}

\item{row.names}{(Optional) Specify if you want to include row names/numbers in the output file.}

\item{na}{(Optional) String to print for NAs. Defaults to an empty/blank string.}
}
\description{
Improved write function. Writes to csv or excel without row names and automatically adds .csv to the file name if it isn't there already. Changes to .csv if a non-xlsx extension is passed.
Easier to type than write.csv(row.names=F) or write.excel().
Author: Bryce Chamberlain. Tech reveiw: Maria Gonzalez.
}
\examples{
# write the cars dataset.
path = paste0(tempdir(), '/out.csv')
w(cars, path)

# cleanup.
file.remove(path)
}
