#
# NAME:
# readuvi
#
# PURPOSE:
# To read collocatiohn files containing UV index estimates and
# observations.
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
# $Id: readuvi.R,v 1.1 2011-04-07 10:14:23 steingod Exp $
#

readuvi <- function(filename) {

    t <- read.table(filename,
	    col.names=c("Year","Month","Day","Hour","Minute",
	    "Station", 
	    "Obs","Clear","Broken","Cloudy","Forecast"))

    mytime <- ISOdatetime(t$Year,t$Month,t$Day,t$Hour,t$Minute,0)

    return(data.frame(year=t$Year,month=t$Month,day=t$Day,hour=t$Hour,min=t$Minute,date=mytime,station=t$Station,observed=t$Obs,estclear=t$Clear,estbroken=t$Broken,estcloud=t$Cloudy,estforecast=t$Forecast))
}
