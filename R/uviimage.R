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
# myparam is yet not working...
# Truncation of the map only works for point mode.
#
# AUTHOR:
# Øystein Godøy, METNO/FOU, 2012-05-10 
#
# MODIFIED:
# NA
#
# CVS_ID:
# $Id: uviimage.R,v 1.4 2012-06-11 20:51:29 steingod Exp $
#

uviimage <- function(myparam="uvic", mydata, projection=FALSE,
mycolors=uvicolors(), mode="image", section=c(-180,180,-90,90)) {

    if (mode=="image") {
        library(lattice)
    } else if (mode=="points") {
        mycolors <- uvicolors()
    }
    library(maps)
    library(mapdata)
    if (projection) {
        #library(mapproj)
        library(proj4)
    }
    myxlim <- c(section[1:2])
    myylim <- c(section[3:4])

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
        if (mode=="image") {
            contourplot(uvic~ucsx*ucsy,
                data=mydata$data,aspect="iso",
                region=T, contour=T, at=c(0,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5),
                panel=function(...,mymap){panel.levelplot(...);
                panel.lines(mymap,col="grey")},
                mymap=mymap, col.regions=mycolors,
                main=paste("UV index estimate for",mydata$validtime))
        }
    } else {
        cat("No map projection...\n")
        if (mode=="image") {
            contourplot(uvic~lon*lat, 
                data=mydata$data,aspect="iso", 
                region=T, contour=T, at=c(0,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5),
                panel=function(...,mymap){panel.levelplot(...);
                panel.lines(mymap,col="grey")},
                mymap=mymap, col.regions=mycolors,
                main=paste("UV index estimate for",mydata$validtime)) 
        } else if (mode=="points") {
            map(fill=TRUE, col="seashell", bg="skyblue",
                interior=FALSE, xlim=myxlim, ylim=myylim)
            myvalues <- round(mydata$data$uvic)
            myvalues <- ifelse(myvalues>11,11,myvalues)
            points(mydata$data$lon,mydata$data$lat,pch=24,bg=mycolors[myvalues],cex=2)
            text(mydata$data$lon,mydata$data$lat,labels=myvalues,cex=0.6)
            title(paste("UV index estimate for",mydata$validtime))
        }
    }
}

