#  setwd("d:\\Kroes\\swap4\\Report2780_Swap4_Wofost_Hydrology\\InstallPackageSwap40\\Cases_CNmodule\\Maize(Cran16)")
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
filnamsim <- c("./result.inc","./result.crp","./result_nut.csv")
filnamobs <- c("./observed/obs.Gwl.csv.txt","./observed/obs.yield.csv.txt",
               "./observed/obs.yieldN.csv.txt","./observed/obs.leachNO3N.csv.txt")


# 1. GWL in m
# --- sim
sim1  <- read.csv(file = filnamsim[1], header = TRUE, as.is = TRUE, skip = 6) 
sim1$Date <- as.Date(sim1$Date)
sim1$gwl  <- 0.01*sim1$Gwl
obs1 <- data.frame(Gwl=rep(NA,length(sim1$Gwl)))
# --- obs
	header    <- readHeader(filnamobs[1])
	obs1      <- read.csv(file = filnamobs[1], header = TRUE, strip.white = TRUE, skip = length(header)) 
	obs1      <- subset(obs1,property=="Gwl")
	obs1$Date <- as.Date(obs1$Date,format="%Y-%m-%d")
	   
# 2. Yield (forage maize:  total above ground biomass CWDM)
# --- sim
sim2      <- read.csv(file = filnamsim[2], header = TRUE, as.is = TRUE, skip = 7) 
sim2$Date <- as.Date(sim2$Date)
sim2$year = 1900 + as.POSIXlt(sim2$Date)$year
yrs       <- unique(sim2$year)
nrofyrs   <- length(yrs)
sim2yr    <- data.frame(year=yrs,yieldact=rep(NA,nrofyrs),yieldpot=rep(NA,nrofyrs))
for (iyr in (1:nrofyrs)) {
	# iyr <- 1
	tmp <- subset(sim2,year==yrs[iyr])
	if(sum(tmp$CWSO,na.rm=TRUE)>0) {
		sim2yr$yieldact[iyr] <- max(tmp$CWDM,na.rm=TRUE) 
		sim2yr$yieldpot[iyr] <- max(tmp$CPWDM,na.rm=TRUE)
	}
}
# --- obs
header    <- readHeader(filnamobs[2])
obs2      <- read.csv(file = filnamobs[2], header = TRUE, strip.white = TRUE, skip = length(header)) 
obs2       <- na.omit(obs2)
obs2$value <- as.double(as.character(obs2$value))
obs2$Date  <- as.Date(obs2$Date,format="%Y-%m-%d")
obs2$year = 1900 + as.POSIXlt(obs2$Date)$year
yrs       <- unique(obs2$year)
nrofyrs   <- length(yrs)
obs2yr    <- data.frame(year=yrs,yield=rep(NA,nrofyrs))
for (iyr in (1:nrofyrs)) {
		obs2yr$yield[iyr] <- sum(subset(obs2,year==yrs[iyr])$value)
}

# 3. Yield of N 
# --- sim
sim3      <- read.csv(file = filnamsim[3], header = TRUE, as.is = TRUE, skip = 6) 
sim3$Date <- as.Date(sim3$Date)
sim3$year = 1900 + as.POSIXlt(sim3$Date)$year
sim3$yieldN <- sim3$NH4_upt + sim3$NO3_upt
yrs       <- unique(sim3$year)
nrofyrs   <- length(yrs)
sim3yr    <- data.frame(year=yrs,yieldN=rep(NA,nrofyrs))
for (iyr in (1:nrofyrs)) {
	sim3yr$yieldN[iyr] <- sum(subset(sim3,year==yrs[iyr])$yieldN)
}
# --- obs yieldN
if (filnamobs[3]!="NA")  {
	header    <- readHeader(filnamobs[3])
	obs3      <- read.csv(file = filnamobs[3], header = TRUE, strip.white = TRUE, skip = length(header)) 
	obs3 <- subset(obs3,property=="CRNTYD")
	obs3$value <- as.double(as.character(obs3$value))
	obs3$Date  <- as.Date(obs3$Date,format="%Y-%m-%d")
	obs3$year = 1900 + as.POSIXlt(obs3$Date)$year
	yrs       <- unique(obs3$year)
	nrofyrs   <- length(yrs)
	obs3yr    <- data.frame(year=yrs,yieldN=rep(NA,nrofyrs))
	for (iyr in (1:nrofyrs)) {
		obs3yr$yieldN[iyr] <- max(subset(obs3,year==yrs[iyr])$value)
	}
}

# 4. Nitrogen balance: leaching of NO3-N in kg/ha N
# --- sim
sim4 <- sim3
sim4$leachNO3N <- sim4$NO3_out   
yrs       <- unique(sim4$year)
nrofyrs   <- length(yrs)
sim4yr    <- data.frame(year=yrs, leachNO3N=rep(NA,nrofyrs),leachH2O=rep(NA,nrofyrs))
for (iyr in (1:nrofyrs)) {
	sim4yr$leachNO3N[iyr] <- sum(subset(sim4,year==yrs[iyr])$leachN)
	sim4yr$leachH2O[iyr] <- sum(subset(sim4,year==yrs[iyr])$WFl_Out)
}
# --- obs leachN concentration of Nitrate-N in mg/l, transformed to kg/ha assuming that sim waterflux is correct.
if (filnamobs[4]!="NA")  {
	header    <- readHeader(filnamobs[4])
	obs4      <- read.csv(file = filnamobs[4], header = TRUE, strip.white = TRUE, skip = length(header)) 
	obs4 <- subset(obs4,property=="CONI")
	obs4$value <- as.double(as.character(obs4$value))
	obs4$Date  <- as.Date(obs4$Date,format="%Y-%m-%d")
	obs4$year = 1900 + as.POSIXlt(obs4$Date)$year
	yrs       <- unique(obs4$year)
	nrofyrs   <- length(yrs)
	obs4yr   <- data.frame(year=yrs,leachNO3Nconc=rep(NA,nrofyrs),leachH2Osim=rep(NA,nrofyrs),leachNO3Nkgpha=rep(NA,nrofyrs))
	for (iyr in (1:nrofyrs)) {
		obs4yr$leachNO3Nconc[iyr] <- mean(subset(obs4,year==yrs[iyr])$value)
		obs4yr$leachH2Osim[iyr] <- mean(subset(sim4yr,year==yrs[iyr])$leachH2O)
		obs4yr$leachNO3Nkgpha[iyr] <- 0.01 * obs4yr$leachNO3Nconc[iyr] * obs4yr$leachH2Osim[iyr]
	}
}

# generate plotfiles
png(file = "PlotResult.png", width = 1024, height = 768, pointsize = 22, bg = "white")
par(mfrow = c(4, 1),mar = c(bottom = 2, left = 4, top = 2, right = 2))

# -- plot 1: Groundwater level
main <- paste("Groundwaterlevel (m-soil surface)")
xlim <- c(xmin=min(sim1$Date), xmax=max(sim1$Date))
ylim <- c(-3.0,0.0)
plot(x=sim1$Date,y=sim1$gwl,type="l",main=main,xlim=xlim,ylim=ylim,lwd=1,xlab="",ylab="")
points(x=obs1$Date,y=obs1$value,col="red",pch=19)
legend("topleft",legend=c("simulated","observed"), 
	cex=0.9, pch=c(-1,19), lty=c(1,0),col=c("black","red"),lwd=c(1,1))
lines(x=sim1$Date,y=sim1$gwl,type="l")

# plot 2: Yield
main <- "Forage maize, Yield as above ground biomass (kg/ha DM)"
ylim <- c(0.0,max(sim2$CPWDM,sim2$CWDM))
plot(x=sim2$Date,y=sim2$CPWDM,type="p",main=main,xlim=xlim,
	cex=0.5,col="green",xlab="",ylab="",lty=1,lwd=1.2,ylim=ylim)
lines(x=sim2$Date,y=sim2$CWDM,type="p",col="black",cex=0.5,lty=1,lwd=1.2)
points(x=obs2$Date,y=obs2$value,col="red",pch=19) 
legend("topleft",legend=c("simulated_pot","simulated_act","observed"), 
	cex=0.9, pch=c(-1,-1,19),lty=c(1,1,0),col=c("green","black","red"),lwd=c(1.2,1.2,1) )

# -- plot 3: N-uptake per year
main <- "Forage maize, Nitrogen uptake by crop (kg/ha N)"
xlim <- c(min(sim3yr$year),max(sim3yr$year))
ylim <- c(0.0,max(sim3yr$yieldN,obs3yr$yieldN,na.rm=TRUE))
plot(x=sim3yr$year,y=sim3yr$yieldN,type="b",main=main,xlim=xlim,ylim=ylim,lwd=1,xlab="",ylab="")
points(x=obs3yr$year,y=obs3yr$yieldN,col="red",pch=19)
legend("bottomleft",legend=c("simulated","observed"), 
	cex=0.9, pch=c(1,19), lty=c(2,0),col=c("black","red"),lwd=c(1,1))

# -- plot 4: NO3N-leaching per year
main <- "Forage maize, Nitrate leaching (kg/ha NO3-N)"
xlim <- c(min(sim4yr$year),max(sim4yr$year))
ylim <- c(0.0,max(sim4yr$leachNO3N,obs4yr$leachNO3Nkgpha,na.rm=TRUE))
plot(x=sim4yr$year,y=sim4yr$leachNO3N,type="b",main=main,xlim=xlim,ylim=ylim,lwd=1,xlab="",ylab="")
points(x=obs4yr$year,y=obs4yr$leachNO3Nkgpha,col="red",pch=19)
legend("bottomleft",legend=c("simulated","observed"), 
	cex=0.9, pch=c(1,19), lty=c(2,0),col=c("black","red"),lwd=c(1,1))

#close plotfile
dev.off()

