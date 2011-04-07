#
# NAME:
# validateuv
#
# PURPOSE:
# To read collocation files and generate statistcs.
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
# $Id: validateuv.R,v 1.1 2011-04-07 10:14:24 steingod Exp $
#
validateuv <- function(filename) {

    t <- read.table(filename,
	    col.names=c("Year","Month","Day","Hour","Minute",
	    "Station", 
	    "Obs","Clear","Broken","Cloudy","Forecast"))

    mytime <- ISOdatetime(t$Year,t$Month,t$Day,t$Hour,t$Minute,0)

    # Forecast
    plot(mytime,t$Obs-t$Forecast,ylab="Observation - ECMWF based Forecast")
    abline(h=0)
    answer <- readline("Would you like to create a PNG [y|n]> ")
    if (substr(answer,1,1)=="y") {
	png(filename="uvval-forecast-%02d.png",width=600,height=600)
	    plot(mytime,t$Obs-t$Forecast,
		    ylab="Observation - ECMWF based Forecast")
	    abline(h=0)
	    dev.off()
    } 

    # Clear
    plot(mytime,t$Obs-t$Clear,ylab="Observation - Clear")
    abline(h=0)
    answer <- readline("Would you like to create a PNG [y|n]> ")
    if (substr(answer,1,1)=="y") {
	png(filename="uvval-clear-%02d.png",width=600,height=600)
	    plot(mytime,t$Obs-t$Clear,
		    ylab="Observation - Clear")
	    abline(h=0)
	    dev.off()
    } 

    # Broken
    plot(mytime,t$Obs-t$Broken,ylab="Observation - Broken")
    abline(h=0)
    answer <- readline("Would you like to create a PNG [y|n]> ")
    if (substr(answer,1,1)=="y") {
	png(filename="uvval-broken-%02d.png",width=600,height=600)
	    plot(mytime,t$Obs-t$Broken,
		    ylab="Observation - Broken")
	    abline(h=0)
	    dev.off()
    } 

    # Overcast
    plot(mytime,t$Obs-t$Cloudy,ylab="Observation - Overcast")
    abline(h=0)
    answer <- readline("Would you like to create a PNG [y|n]> ")
    if (substr(answer,1,1)=="y") {
	png(filename="uvval-overcast-%02d.png",width=600,height=600)
	    plot(mytime,t$Obs-t$Cloudy,
		    ylab="Observation - Overcast")
	    abline(h=0)
	    dev.off()
    } 

    # Stations
    plot(t$Station,t$Obs-t$Forecast,ylab="Observed - Estimated")
    abline(h=0)
    answer <- readline("Would you like to create a PNG [y|n]> ")
    if (substr(answer,1,1)=="y") {
	png(filename="uvval-stations-%02d.png",width=600,height=600)
	    plot(t$Station,t$Obs-t$Cloudy,
		    ylab="Observed -Estimated")
	    abline(h=0)
	    dev.off()
    } 

    cat()
}
