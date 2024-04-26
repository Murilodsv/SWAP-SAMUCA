#  setwd("d:\\Kroes\\swap4\\Report2780_Swap4_Wofost_Hydrology\\InstallPackageSwap40\\cases\\3.MacroporeFlow")
# --- read libs
#library(RSWAP)
library(hydroGOF)      # hydro-packages by Mauricio Zambrano-Bigiarini [aut, cre]
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
t1 <- date()
#plotfiletype<-"pdf"
plotfiletype <-"png"
textfact     <- 0.8

# read filenames and croptype from file
tmp <- read.csv(file = "obs.filenames.txt", header = TRUE, as.is = TRUE, skip = 2) 
filnamobs <- delSpaces(tmp$filnamobs)
croptype  <- delSpaces(tmp$landuse[1])

# --- define 8 performance indices with stats
# with GOF-functions: Mean Error (ME), Root Mean Square Error (RMSE), 
#                     Nash-Sutcliffe efficiency (NSE),  Index of Agreement (d)
PIname    <- c("Yield",     "LAI",  "ETact","Gwl",     "Theta20cm","PresHead20cm","qDrain","qBot","yieldN","leachN")
PIunit    <- c("kg/ha/yr DM","m2/m2","mm/yr","m-soilsurface","m3/m3","cm","mm","mm","kg/ha/yr N","kg/ha/yr N")
# --- initialise stats
stats <- data.frame(PIname=PIname,PIunit=PIunit,SIMmean=NA,OBSmean=NA,ME=NA,RMSE=NA,NSE=NA,d=NA)

# --- read SWAP's log-file
datlog <- readLines(con = "andelst_swap.log")

# names of files with simulation results
filnamsim <- c("./result.crp","./result.inc")

# -------------------- GWL in m
# --- sim
sim4  <- read.csv(file = filnamsim[2], header = TRUE, as.is = TRUE, skip = 6) 
sim4$Date <- as.Date(sim4$Date)
sim4$gwl_m  <- 0.01*sim4$Gwl
obs4 <- data.frame(Gwl=rep(NA,length(sim4$Gwl)))
# --- obs
if (filnamobs[4]!="NA")  {
	header    <- readHeader(filnamobs[4])
	obs4      <- read.csv(file = filnamobs[4], header = TRUE, strip.white = TRUE, skip = length(header)) 
	obs4      <- subset(obs4,property=="Gwl")
	obs4$Date <- as.Date(obs4$Date,format="%Y-%m-%d")
#	obs4 <- na.omit(obs4)
    mer              <- merge(x = sim4[,c(1,ncol(sim4))], y = obs4[,c(1,3)], by.x = "Date", by.y = "Date",rm.na=TRUE)
    if(nrow(mer)>0) {
		stats$SIMmean[4] <- round(mean(mer$gwl),3)
		stats$OBSmean[4] <- round(mean(mer$value),2)
		stats$ME[4]      <- round(gof(mer$gwl,mer$value,rm.NA=TRUE)[1],3)
		stats$RMSE[4]    <- round(gof(mer$gwl,mer$value,rm.NA=TRUE)[4],3)
		stats$NSE[4]     <- round(gof(mer$gwl,mer$value,rm.NA=TRUE)[9],3)
		stats$d[4]       <- round(gof(mer$gwl,mer$value,rm.NA=TRUE)[12],3)
	}
}

# --------------------- qDrainage in mm
# --- sim
sim7      <- read.csv(file = filnamsim[2], header = TRUE, as.is = TRUE, skip = 6) 
sim7$Date <- as.Date(sim7$Date)
sim7$year = 1900 + as.POSIXlt(sim7$Date)$year
sim7$qDrain_mm <- 10*(sim7$Drainage)
yrs       <- unique(sim7$year)
nrofyrs   <- length(yrs)
sim7yr    <- data.frame(year=yrs,qDrain=rep(NA,nrofyrs))
for (iyr in (1:nrofyrs)) {
   sim7yr$qDrain[iyr] <- sum(subset(sim7,year==yrs[iyr])$qDrain_mm)
}
stats$SIMmean[7] <- round(mean(sim7$qDrain_mm),2)
# --- obs
obs7 <- data.frame(qdrain=rep(NA,length(sim7$Drainage)))
if (filnamobs[7]!="NA")  {
	header    <- readHeader(filnamobs[7])
	obs7      <- read.csv(file = filnamobs[7], header = TRUE, strip.white = TRUE, skip = length(header)) 
#	obs7      <- subset(obs7,property=="SumDRDCD1")
#	obs7      <- subset(obs7,property=="SumDRDCD2")
	obs7$Date <- as.Date(obs7$Date,format="%Y-%m-%d")
	obs7      <- na.omit(obs7)
	obs7$year = 1900 + as.POSIXlt(obs7$Date)$year
	yrs       <- unique(obs7$year)
	nrofyrs   <- length(yrs)
	obs7yr    <- data.frame(year=yrs,qdrain=rep(NA,nrofyrs))
	for (iyr in (1:nrofyrs)) {
	   obs7yr$qdrain[iyr] <- sum(subset(obs7,year==yrs[iyr])$value)
	}
    mer              <- merge(x = sim7[,c(1,ncol(sim7))], y = obs7[,c(1,3)], by.x = "Date", by.y = "Date",rm.na=TRUE)
	stats$SIMmean[7] <- round(mean(mer$qDrain_mm),2)
	stats$OBSmean[7] <- round(mean(mer$value),2)
	stats$ME[7]      <- round(gof(mer$qDrain_mm,mer$value,rm.NA=TRUE)[1],2)
	stats$RMSE[7]    <- round(gof(mer$qDrain_mm,mer$value,rm.NA=TRUE)[4],2)
	stats$NSE[7]     <- round(gof(mer$qDrain_mm,mer$value,rm.NA=TRUE)[9],2)
	stats$d[7]       <- round(gof(mer$qDrain_mm,mer$value,rm.NA=TRUE)[12],2)
}


# generate plotfiles

# ----- 
png(file = "PerformIndex.png", width = 1024, height = 768, pointsize = 22, bg = "white")
par(mfrow = c(2, 1),mar = c(bottom = 2, left = 4, top = 2, right = 2))
# -- plot  Gwl
main <- paste(stats$PIname[4],"; ",names(stats[3]),stats$SIMmean[4],"; ",
		names(stats[4]),stats$OBSmean[4],"; ",names(stats[5]),stats$ME[4],"; ",PIunit[4])
xlim <- c(xmin=min(sim4$Date), xmax=max(sim4$Date))
ylim <- c(-3.0,0.0)
if(!is.na(sum(obs4$value)))            { ylim <- c(min(sim4$gwl,na.rm=TRUE), 0.0) }
if(!is.na(sum(sim4$gwl,obs4$value)))   { ylim <- c(min(sim4$gwl,obs4$value,na.rm=TRUE),0.0) }
plot(x=sim4$Date,y=sim4$gwl,type="l",main=main,xlim=xlim,ylim=ylim,lwd=1,xlab="",ylab="",cex.main = textfact)
if (filnamobs[4]!="NA")  {
	points(x=obs4$Date,y=obs4$value,col="red",pch=19)
	legend("topleft",legend=c("simulated","observed"), 
		cex=0.9, pch=c(-1,19), lty=c(1,0),col=c("black","red"),lwd=c(1,1))
	lines(x=sim4$Date,y=sim4$gwl,type="l")
}
# -- plot  qdrain
ylim <- c(min(sim7$qDrain_mm),max(sim7$qDrain_mm))
main <- paste(stats$PIname[7],"; ",names(stats[3]),stats$SIMmean[7],"; ",
		names(stats[4]),stats$OBSmean[7],"; ",names(stats[5]),stats$ME[7],"; ",PIunit[7])
plot(x=sim7$Date,y=sim7$qDrain_mm,type="l",main=main,xlim=xlim,ylim=ylim,
		xlab="",ylab="",cex.main = textfact)
if (filnamobs[7]!="NA")  {
	points(x=obs7$Date,y=obs7$value,col="red",pch=19)
	legend("topleft",legend=c("simulated","observed"), 
		cex=0.9,pch=c(-1,19), lty=c(1,0),col=c("black","red"))
}
# close plotfile
dev.off()
