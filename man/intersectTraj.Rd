% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cqmaTools.R
\name{intersectTraj}
\alias{intersectTraj}
\title{Intersect trajectories}
\usage{
intersectTraj(file.vec, limit.in, cnames, srs)
}
\arguments{
\item{file.vec}{A character vector. The paths to the input files}

\item{limit.in}{A SpatialLinesDataFrame object which is used to intersect the trajectories.}

\item{cnames}{A character vector. The name of the columns of the raw data file (hysplit)}

\item{srs}{A length-1 character. The spatial reference system}
}
\value{
A list made of a character vector and a list. The character vector is the path to each trajectory file. The list contains the first row in the trajectory file which lies over the sea
}
\description{
Find trajectories' first point beyond the limit (e.g. over the sea).
}
\author{
Alber Sanchez, \email{alber.ipia@inpe.br}
}
