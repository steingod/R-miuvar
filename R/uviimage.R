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
# $Id: uviimage.R,v 1.1 2012-05-22 11:19:36 steingod Exp $
#

uviimage <- function(myparam="uvic", mydata, projection=FALSE, mycolors=) {

    library(lattice)
    library(maps)
    library(mapdata)
    if (projection) {
        library(mapproj)
    }

    if (projection) {
        mymap <- map("worldHires",plot=F, projection="stereographic", 
                orientation=c(60, 0, 0))
        mynewcoord <- mapproject(mydata$data$lon, mydata$data$lat,
                projection="stereographic", orientation=c(60,0,0))
        mydata$data$ucsx <- round(mynewcoord$x,digits=2)
        mydata$data$ucsy <- round(mynewcoord$y,digits=2)
        cat("Using map projection...\n")
##        mycols <- sort(unique(mydata$data$ucsy))
##        myrows <- sort(unique(mydata$data$ucsx))
##        myncol <- length(mycols)
##        mynrow <- length(myrows)
##        mymat <- matrix(NA, nrow=mynrow, ncol=myncol,
##                dimnames=list(
##                    formatC(myrows, format="f", digits=2),
##                    formatC(mycols, format="f", digits=2))) 
##        myindx <- match(formatC(mydata$data$ucsy,format="f",digits=2),colnames(mymat))
##        myindy <- match(formatC(mydata$data$ucsx,format="f",digits=2),rownames(mymat))
##        myindmat <- cbind(myindy,myindx)
##        myindmattmp <- myindmat[! is.na(myindmat[,2]),]
##        myindmat <- myindmattmp
        #return(data.frame(myindy, myindx, lat=mydata$data$lat,
        #            lon=mydata$data$lon, ucsy=mydata$data$ucsy, ucsx=mydata$data$ucsx))
##        mymat[myindmat] <- mydata$data$uvic
    } else {
        mymap <- map("worldHires",plot=F)
        cat("No map projection...\n")
##        mycols <- sort(unique(mydata$data$lat))
##        myrows <- sort(unique(mydata$data$lon))
##        mynrow <- length(myrows)
##        myncol <- length(mycols)
##        mymat <- matrix(NA, nrow=mynrow, ncol=myncol,
##                dimnames=list(
##                    formatC(myrows, format="f", digits=2),
##                    formatC(mycols, format="f", digits=2))) 
##        myindx <- match(formatC(mydata$data$lat,format="f",digits=2),colnames(mymat))
##        myindy <- match(formatC(mydata$data$lon,format="f",digits=2),rownames(mymat))
##        myindmat <- cbind(myindy,myindx)
##        mymat[myindmat] <- mydata$data$uvic
    }
    #image(myrows,mycols,mymat)
    #return(mymat)

    if (projection) {
        cat("Using map projection...\n")
        levelplot(uvic~ucsx*ucsy,
                data=mydata$data,aspect="iso",
                region=T, contour=F,
                panel=function(...,mymap){panel.levelplot(...); panel.lines(mymap$x,mymap$y,col="black")},
                mymap=mymap, col.regions=heat.colors(100))
    } else {
        cat("No map projection...\n")
        levelplot(uvic~lon*lat, 
                data=mydata$data,aspect="iso", 
                panel=function(...,mymap){panel.levelplot(...); panel.lines(mymap$x,mymap$y,col="black")},
                mymap=mymap, col.regions=heat.colors(100)) 
    }
    #title(paste("UV index estimate for",mydata$validtime))
    #return(mydata)
}

