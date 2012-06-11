\name{readuvi}

\alias{readuvi}

\title{
    Read UV index estimates
}

\description{
    Reads ASCII files containing UV index estimates generated using
    libRadtran at the Norwegian Meteorological institute. 
    
    When mode is Full, each record contains a name for the position, while
    Minimal does not contain this. Usually Full is used for point
    estimates while Minimal is used for gridded estimates.
}

\usage{
    readuvi(filename, mode="Minimal")
}

\arguments{
    \item{filename}{Collocation file to read}
    \item{mode}{Minimal or Full}
}

\value{
    \link{list} with validtime and data as \link{data.frame}
}

\seealso{
    \link{readuvival}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
}

\keyword{
}
