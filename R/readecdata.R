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
# $Id: readecdata.R,v 1.3 2012-06-11 20:51:29 steingod Exp $
#
readecdata <- function(file, plot=TRUE, myfield=4, proj="latlon",
mycolors=heat.colors(100)) {

    library(RNetCDF)
    library(maps)
    nc <- open.nc(file)

    print.nc(nc)

    myuvmed <- NULL
    mytempssi <- NULL
    mytempdli <- NULL

##    mytitle <- att.get.nc(nc,"NC_GLOBAL","title")
##    myplatform <- att.get.nc(nc,"NC_GLOBAL","Platform_name")
##    mystartdate <- att.get.nc(nc,"NC_GLOBAL","start_date")
##    mystopdate <- att.get.nc(nc,"NC_GLOBAL","stop_date")
    if (proj=="polster") {
        mylatitude <- var.get.nc(nc,"latitude")
        require(osisaf)
        data(osisafmapdata)
        mymap <- 1000*latlon2ucs(osisafmapdata$lat,osisafmapdata$lon)
    } else {
        mylatitude <- rev(var.get.nc(nc,"latitude"))
    }
    mylongitude <- var.get.nc(nc,"longitude")
    tmp <- ifelse(mylongitude>180,mylongitude-360,mylongitude)
    myind <- which(tmp==0)
    mylen <- length(tmp)
    mylen1 <- length(tmp[tmp<0])
    mylen2 <- length(tmp[tmp>=0])
    mylongitude[1:mylen1] <- tmp[(mylen2+1):(mylen2+mylen1)]
    mylongitude[(mylen1+1):(mylen1+mylen2)] <- tmp[1:(mylen2)]
    #return(list(mylen,myind,mylen1,mylen2,mylongitude))
    
    myozone <- var.get.nc(nc,"tco3")/2.1415e-5
    tmp <- myozone
    myozone[1:mylen1,,] <- tmp[(mylen2+1):(mylen2+mylen1),,]
    myozone[(mylen1+1):(mylen1+mylen2),,] <- tmp[1:(mylen2),,]

    if (proj=="polster" || proj=="latlon") {
        myxmaplim <- c(min(mylongitude),max(mylongitude))
        myymaplim <- c(min(mylatitude),max(mylatitude))
        myxdim <- myxmaplim[2]-myxmaplim[1]
        myydim <- myymaplim[2]-myymaplim[1]
        aspectratio <- myxdim/myydim
    }

    myvaltime <- ISOdatetime(1970,1,1,0,0,0,tz="GMT")+(var.get.nc(nc,"time"))
    myreftime <- ISOdatetime(1970,1,1,0,0,0,tz="GMT")+(var.get.nc(nc,"forecast_reference_time"))

    close.nc(nc)

    if (plot==TRUE) {
        par(pty="m")
        if (proj=="polster" || proj=="latlon") {
            #par(plt=c(0.5-(aspectratio/2),0.5+(aspectratio/2),0,1))
            mypin <- par()$pin
            if (aspectratio > 1) {
                mypin[2] <- mypin[2]/aspectratio
            }
            par(pin=mypin)
        }
        if (proj=="polster") {
            image(mylongitude,mylatitude,myozone[,,myfield],col=mycolors,xaxt="n",yaxt="n")
            contour(mylongitude,mylatitude,myozone[,,myfield],add=T)
        } else {
            image(mylongitude,mylatitude,myozone[,length(mylatitude):1,myfield],col=mycolors,xlab="Longitude",ylab="Latitude")
            contour(mylongitude,mylatitude,myozone[,length(mylatitude):1,myfield],add=T)
        }
        if (proj=="polster") {
            lines(mymap$eastings,mymap$northings,new=TRUE)
        } else {
            #map(xlim=c(min(mylongitude),max(mylongitude)),ylim=c(min(mylatitude),max(mylatitude)),add=T,col="black")
            mymap <- map(plot=F)
            lines(mymap$x,mymap$y,new=TRUE,lwd=2)
        }
        title(paste("Total ozone amount in Dobson Units for",
        strftime(myvaltime[myfield],"%F %H:%M UTC",tz="GMT")), 
        sub=paste("ECMWF forecast ",
        strftime(myreftime[myfield],"%F")))
    }

    return(list(reftime=myreftime,valtime=myvaltime,lat=mylatitude,lon=mylongitude,ozone=myozone))
}
