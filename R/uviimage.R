#
# NAME:
# NA
#
# PURPOSE:
# To visualise a map containing UV index estimates.
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
# Several approaches have been tried. Graphical solutions depending on
# Lattice and ordinary base graphics without a clear determination of
# which solution that is the best. For the projected data, there is a risk
# of getting squeezed to invisible.
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, METNO/FOU, 2012-05-10 
#
# MODIFIED:
# NA
#
# CVS_ID:
# $Id: uviimage.R,v 1.2 2012-05-23 08:15:32 steingod Exp $
#

uviimage <- function(myparam="uvic", mydata, projection=FALSE,
mycolors=heat.colors(100)) {

    library(lattice)
    library(maps)
    library(mapdata)
    if (projection) {
        #library(mapproj)
        library(proj4)
    }

    mymap <- map("worldHires",plot=F)
    tmpmat <- cbind(x=mymap$x, y=mymap$y)
    if (projection) {
        mymap <- project(tmpmat,proj="+proj=stere +lat_ts=60 +lat_0=90 +lon_0=0 +a=6371 +b=6371 +units=km")
        mynewcoord <- project(list(mydata$data$lon, mydata$data$lat),proj="+proj=stere +lat_ts=60 +lat_0=90 +lon_0=0 +a=6371 +b=6371 +units=km")
                
        mydata$data$ucsx <- round((round(mynewcoord$x*100/3)*3)/100,digits=2)
        mydata$data$ucsy <- round((round(mynewcoord$y*100/3)*3)/100,digits=2)
        cat("Using map projection...\n")
    } else {
        cat("No map projection...\n")
        mymap <- tmpmat
    }

    if (projection) {
        cat("Using map projection...\n")
        contourplot(uvic~ucsx*ucsy,
                data=mydata$data,aspect="iso",
                region=T, contour=T, at=c(0,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5),
                panel=function(...,mymap){panel.levelplot(...);
                panel.lines(mymap,col="grey")},
                mymap=mymap, col.regions=mycolors,
                main=paste("UV index estimate for",mydata$validtime))
    } else {
        cat("No map projection...\n")
        contourplot(uvic~lon*lat, 
                data=mydata$data,aspect="iso", 
                region=T, contour=T, at=c(0,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5),
                panel=function(...,mymap){panel.levelplot(...);
                panel.lines(mymap,col="grey")},
                mymap=mymap, col.regions=mycolors,
                main=paste("UV index estimate for",mydata$validtime)) 
    }
}

