#
# NAME:
# NA
#
# PURPOSE:
# NA
#
# REQUIREMENTS:
# NA
#
# INPUT:
# NA
#
# OUTPUT:
# NA
#
# NOTES:
# NA
#
# BUGS:
# NA
#
# AUTHOR:
# NA
#
# MODIFIED:
# NA
#
# CVS_ID:
# $Id: plotval.R,v 1.1 2011-04-07 10:14:24 steingod Exp $
#

plotval <- function(x, estimate="FC", station=NULL) {

    if (estimate == "FC") {
        if (is.null(station)) {
            xval <- x$date
            yval <- x$observed - x$estforecast
        } else {
            xval <- x$date[x$station == station]
            yval <- x$observed[x$station == station] - x$estforecast[x$station == station]
        }
        xlab=""
        ylab="Observed - Estimated"
        mytitle="ECMWF forecasted cloud cover"
    } else if (estimate == "CL") {
        if (is.null(station)) {
            xval <- x$date
            yval <- x$observed - x$estclear
        } else {
            xval <- x$date[x$station == station]
            yval <- x$observed[x$station == station] - x$estclear[x$station == station]
        }
        xlab=""
        ylab="Observed - Estimated"
        mytitle="Clear sky estimates"
    } else if (estimate == "BR") {
        if (is.null(station)) {
            xval <- x$date
            yval <- x$observed - x$estbroken
        } else {
            xval <- x$date[x$station == station]
            yval <- x$observed[x$station == station] - x$estbroken[x$station == station]
        }
        xlab=""
        ylab="Observed - Estimated"
        mytitle="Partly cloudy sky estimates"
    } else if (estimate == "OV") {
        if (is.null(station)) {
            xval <- x$date
            yval <- x$observed - x$estcloud
        } else {
            xval <- x$date[x$station == station]
            yval <- x$observed[x$station == station] - x$estcloud[x$station == station]
        }
        xlab=""
        ylab="Observed - Estimated"
        mytitle="Cloudy sky estimates"
    }

    plot(xval,yval,xlab=xlab,ylab=ylab)
    abline(h=0)
    meanbias <- mean(yval)
    stdev <- sd(yval)
    abline(h=c(meanbias,meanbias+2*stdev,meanbias-2*stdev),col=c(2,3,3))
    mtext(formatC(meanbias,digits=2,format="f"),2,at=meanbias,las=1)
    title(mytitle)
}
