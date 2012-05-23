\name{uviimage}

\alias{uviimage}

\title{
    Plot a map of the estimated UV index
}

\description{
    Plots a map of the estimated UV index. This function offers the
    possibility to plot the estimated UV index using the raw geographical
    coordinates (latitude, longitude) or a map projected version.
    Currently only a Polar Stereographic map projection using a sphere
    with radius of 6371 km is supprted through the \link{proj4} package.
    Maps are collected from the \link{maps} package although the datum
    used for this is uncertain. The result is currently not smoothed.
}

\usage{
    uviimage(myparam="uvic", mydata, projection=FALSE, mycolors=heat.colors(100)
}

\arguments{
    \item{myparam}{which parameter in the input \link{data.frame} to use.
    Currently only the clear sky index is supported.}
    \item{mydata}{the \link{data.frame} containing input data as returned
    by \link{readuvi}}.
    \item{projection}{boolean specifying whether the data should be map
    projected or not. Should be changed to a proj4 string in the future.}
    \item{mycolors}{the colors to use in the plot. This could by colors
    returned by \link{rainbow}, \link{heat.colors} or the default
    \link{uvicolors}}.
}

\value{
    None
}

\seealso{
    \link{readuvi}, \link{uvicolors}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    tmp <- readuvi("myuvifile.txt")
    uviimage(mydata=tmp, proj=T)
}

\keyword{
}
