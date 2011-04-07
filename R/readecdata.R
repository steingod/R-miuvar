#!/usr/bin/perl -w
#
# NAME:
# readecdata
#
# PURPOSE:
# To read and visualise the ozone fields being used for UV-index
# generation. These fields are collected from ECMWF in GRIB format,
# converted to NetCDF and the NetCDF file is subsequently read into R.
#
# REQUIREMENTS:
# RNetCDF
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
# Øystein Godøy, METNO/FOU, 2011-04-07 
#
# MODIFIED:
# NA
#
# CVS_ID:
# $Id: readecdata.R,v 1.1 2011-04-07 10:14:24 steingod Exp $
#
readecdata <- function(file, plot=TRUE, myfield=4) {

    library(RNetCDF)
    nc <- open.nc(file)

    print.nc(nc)

    myuvmed <- NULL
    mytempssi <- NULL
    mytempdli <- NULL

##    mytitle <- att.get.nc(nc,"NC_GLOBAL","title")
##    myplatform <- att.get.nc(nc,"NC_GLOBAL","Platform_name")
##    mystartdate <- att.get.nc(nc,"NC_GLOBAL","start_date")
##    mystopdate <- att.get.nc(nc,"NC_GLOBAL","stop_date")
    mylatitude <- var.get.nc(nc,"lat")
    mylongitude <- var.get.nc(nc,"lon")

    myozone <- var.get.nc(nc,"TCO3_sfc")/2.1415e-5

    myvaltime <- ISOdatetime(1992,1,1,0,0,0,tz="GMT")+(var.get.nc(nc,"valtime")*3600)
    myreftime <- ISOdatetime(1992,1,1,0,0,0,tz="GMT")+(var.get.nc(nc,"reftime")*3600)

    close.nc(nc)

    if (plot==TRUE) {
        par(pty="s")
        image(mylongitude,mylatitude,myozone[,,myfield],col=rainbow(12),xlab="Longitude",ylab="Latitude")
        contour(mylongitude,mylatitude,myozone[,,myfield],add=T)
        map(xlim=c(min(mylongitude),max(mylongitude)),ylim=c(min(mylatitude),max(mylatitude)),add=T,col="black")
        title(paste("Total ozone amount in Dobson Units for",
        strftime(myvaltime[myfield],"%F %H:%M UTC",tz="GMT")), 
        sub=paste("ECMWF forecast ",
        strftime(myreftime[myfield],"%F")))
    }

    return(list(reftime=myreftime,valtime=myvaltime,lat=mylatitude,lon=mylongitude,ozone=myozone))
}
