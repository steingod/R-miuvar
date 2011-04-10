#
# NAME:
# readuvi
#
# PURPOSE:
# To read ASCII files containing UV index estimates.
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
# Øystein Godøy, METNO/FOU, 19.10.2010
#
# MODIFIED:
# NA
#
# CVS_ID:
# $Id: readuvi.R,v 1.2 2011-04-10 18:29:38 steingod Exp $
#

readuvi <- function(filename) {

    mytimestring <- scan(filename,skip=1,nlines=1,sep=":",
            what=list("character","character"),nmax=2)
    mytime <- strptime(mytimestring[2]," %Y%m%d%H%M%S",tz="GMT")
    t <- read.table(filename,
	    col.names=c("lat","lon","uvic","uvipoc","uvioc","uvifcc",
	    "ozone","snow","cloud","albedo","altitude","soz"),
            skip=4)

    return(list(validtime=mytime,data=t))
}
