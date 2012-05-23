\name{readecdata}

\alias{readecdata}

\title{
    Read and visualise ozone from ECMWF
}

\description{
    Ozone forecasts are collected from ECMWF as GRIB files. These are
    converted into NetCDF. This function reads and visualises the total
    ozone column. ECMWF provides this information as kg/m², this function
    converts this measure to Dobson Units.
}

\usage{
    readecdata(file,plot=TRUE,myfield)
}

\arguments{
    \item{file}{file (including path) to be read}
    \item{plot}{defaults to \code{TRUE}, generates a contoured map}
    \item{myfield}{each file contains 4 time steps (numbered 1 through 4),
    this field specifies which field to plot}
}

\value{
    Returns a \link{list} containing all the data of the file as reference
    time, valid time, latitude and longitude and the ozone amount in
    Dobson Units.
}

\seealso{
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
}

\keyword{
}
