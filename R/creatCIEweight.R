#
# $Id: creatCIEweight.R,v 1.1 2011-04-07 10:14:24 steingod Exp $
#
createCIEweight <- function() {
    lambda <- seq(250,400,1)
    cie <- vector(length=length(lambda))

    cie1 <- rep(1,length(lambda[lambda<=298]))
    cie2 <- 10^(0.094*(298-lambda[lambda>298&lambda<=328]))
    cie3 <- 10^(0.015*(139-lambda[lambda>328&lambda<=400]))
    
    plot(lambda,c(cie1,cie2,cie3), 
    type="l", col=4, lwd=3,
    xlab="Wavelength [nm]", ylab="")
    abline(v=c(298,328))
    title("CIE UV weight function")
}
