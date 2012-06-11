#
# NAME:
# compareprofiles
#
# PURPOSE:
# To compare various atmospheric profiles.
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
# $Id: compareprofiles.R,v 1.1 2012-06-11 20:51:29 steingod Exp $
#
compareprofiles <- function() {

    afglusdat <- read.table(file="/disk1/software/libRadtran-1.6-beta/data/atmmod/afglus.dat",skip=2,col.names=c("z","p","t","dens","o3","o2","h2o","co2","no2"))
    afglmsdat <- read.table(file="/disk1/software/libRadtran-1.6-beta/data/atmmod/afglms.dat",skip=2,col.names=c("z","p","t","dens","o3","o2","h2o","co2","no2"))
    afglmwdat <- read.table(file="/disk1/software/libRadtran-1.6-beta/data/atmmod/afglmw.dat",skip=2,col.names=c("z","p","t","dens","o3","o2","h2o","co2","no2"))
    afglssdat <- read.table(file="/disk1/software/libRadtran-1.6-beta/data/atmmod/afglss.dat",skip=2,col.names=c("z","p","t","dens","o3","o2","h2o","co2","no2"))
    afglswdat <- read.table(file="/disk1/software/libRadtran-1.6-beta/data/atmmod/afglsw.dat",skip=2,col.names=c("z","p","t","dens","o3","o2","h2o","co2","no2"))
    afgltdat <- read.table(file="/disk1/software/libRadtran-1.6-beta/data/atmmod/afglt.dat",skip=2,col.names=c("z","p","t","dens","o3","o2","h2o","co2","no2"))

    ecmwfdat <- read.table(file="software/uvpos/extractecdata/myfile.txt",skip=2,col.names=c("z","p","t","dens","o3"))

    split.screen(c(1,2))

    plot(afglusdat$t,afglusdat$z,type="l",lwd=3,col=2,xlab="Temperature",ylab="z(km)",xlim=c(160,370))
    lines(afglmsdat$t,afglmsdat$z,col=3,lwd=3)
    lines(afglmwdat$t,afglmwdat$z,col=4,lwd=3)
    lines(afglssdat$t,afglssdat$z,col=5,lwd=3)
    lines(afglswdat$t,afglswdat$z,col=6,lwd=3)
    lines(afgltdat$t,afgltdat$z,col=7,lwd=3)
    lines(ecmwfdat$t,ecmwfdat$z,col=8,lwd=3)
    legend(170,120,
    c("AFGLUS","AFGLMS","AFGLMW","AFGLSS","AFGLSW","AFGLT","ECMWF"),
    col=seq(2,8),lty=1)
    
    screen(2)
    plot(afglusdat$o3,afglusdat$z,type="l",lwd=3,col=2,xlab="Ozone",ylab="z(km)",xlim=c(0,6e+12))
    lines(afglmsdat$o3,afglmsdat$z,col=3,lwd=3)
    lines(afglmwdat$o3,afglmwdat$z,col=4,lwd=3)
    lines(afglssdat$o3,afglssdat$z,col=5,lwd=3)
    lines(afglswdat$o3,afglswdat$z,col=6,lwd=3)
    lines(afgltdat$o3,afgltdat$z,col=7,lwd=3)
    lines(ecmwfdat$o3,ecmwfdat$z,col=8,lwd=3)
    legend(0,120,
    c("AFGLUS","AFGLMS","AFGLMW","AFGLSS","AFGLSW","AFGLT","ECMWF"),
    col=seq(2,8), lty=1)

}
