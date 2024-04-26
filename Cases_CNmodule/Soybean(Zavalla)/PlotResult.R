#  setwd("d:\\Kroes\\swap4\\Report2780_Swap4_Wofost_Hydrology\\InstallPackageSwap40\\Cases_N\Soybean(Zavalla35e)")
# --- read libs
# function to read header of observation files
readHeader <- 
function(filename) {
    grep(
        pattern = "^#", 
        x = readLines(con = filename, warn = FALSE), 
        value = TRUE
    )
}
# function to delete spaces
delSpaces                 <- function(Line){
          #---------------------------------------------------------------------------------------------
          # FUNCTION delSpaces
          #
          #  SYNOPSIS:
          #    - Line        : characterstring
          #
          #  DESCRIPTION:
          #    delete double spaces and last spaces in characterstring
          #---------------------------------------------------------------------------------------------
          library(stringr)
          Line <- gsub(pattern="\t",replacement=" ",x=Line)
          Line <- str_trim(Line)
          while(length(grep(pattern ="  ",x=Line)) != 0) Line <- gsub(pattern="  ",replacement = " ", x=Line)
          return(Line)
          }
# --- define fixed values
wd <- getwd()
plotfiletype <-"png"

# names of files with simulation results
filnamsim <- c("./result.crp","./result.inc")
filnamobs <- c("./observed/obs.yield.csv.txt")

# 1. Yield
# --- sim
sim1      <- read.csv(file = filnamsim[1], header = TRUE, as.is = TRUE, skip = 7) 
sim1$Date <- as.Date(sim1$Date)
sim1$year = 1900 + as.POSIXlt(sim1$Date)$year
yrs       <- unique(sim1$year)
nrofyrs   <- length(yrs)
sim1yr    <- data.frame(year=yrs,yield=rep(NA,nrofyrs),yieldpot=rep(NA,nrofyrs))
for (iyr in (1:nrofyrs)) {
	# iyr <- 1
	tmp <- subset(sim1,year==yrs[iyr])
	if(sum(tmp$CWSO,na.rm=TRUE)>0) {
		sim1yr$yield[iyr]    <- max(tmp$CWSO,na.rm=TRUE) 
		sim1yr$yieldpot[iyr] <- max(tmp$CPWSO,na.rm=TRUE)
	}
}
# --- obs
if (filnamobs!="NA")  {
	header    <- readHeader(filnamobs)
	obs1      <- read.csv(file = filnamobs, header = TRUE, strip.white = TRUE, skip = length(header)) 
	obs1       <- na.omit(obs1)
	obs1$value <- as.double(as.character(obs1$value))
	obs1$Date  <- as.Date(obs1$Date,format="%Y-%m-%d")
	obs1$year = 1900 + as.POSIXlt(obs1$Date)$year
	yrs       <- unique(obs1$year)
	nrofyrs   <- length(yrs)
	obs1yr    <- data.frame(year=yrs,yield=rep(NA,nrofyrs))
	for (iyr in (1:nrofyrs)) {
			obs1yr$yield[iyr] <- sum(subset(obs1,year==yrs[iyr])$value)
	}
}

# generate plotfiles
png(file = "PlotResult.png", width = 1024, height = 768, pointsize = 22, bg = "white")
#   plot  Yield
main <- "Soybean, Yield (kg/ha DM)"
xlim <- c(xmin=min(sim1$Date,obs1$Date), xmax=max(sim1$Date,obs1$Date))
ylim <- c(0.0,max(sim1$CPWSO,sim1$CWSO))
plot(x=sim1$Date,y=sim1$CPWSO,type="l",main=main,xlim=xlim,
	cex=0.5,col="green",xlab="",ylab="",lty=1,lwd=1.2,ylim=ylim)
lines(x=sim1$Date,y=sim1$CWSO,type="l",col="black",cex=0.5,lty=1,lwd=1.2)
if (filnamobs!="NA")  {
	points(x=obs1$Date,y=obs1$value,col="red",pch=19) 
	legend("topleft",legend=c("simulated_pot","simulated_act","observed"), 
		cex=0.9, pch=c(-1,-1,19),lty=c(1,1,0),col=c("green","black","red"),lwd=c(1.2,1.2,1) )
}
# close plotfile
dev.off()

