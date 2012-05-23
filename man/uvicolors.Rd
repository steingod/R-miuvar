\name{uvicolors}

\alias{uvicolors}

\title{
    Create a WHO compliant color scheme for UV index
}

\description{
    Creates the color scheme recommended by the World Health organisation
    (WHO) for UV index.
}

\usage{
    uvicolors()
}

\arguments{
}

\value{
    A character vector of color names.
}

\seealso{
    \link{uviimage}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    uviimage(mydata=tmp,proj=T,mycolors=uvicolors())
}

\keyword{
}
