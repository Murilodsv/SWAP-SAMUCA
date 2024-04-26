#  setwd("d:\\Kroes\\swap4\\Report2780_Swap4_Wofost_Hydrology\\InstallPackageSwap40\\cases\\6.SurfaceWater\\")
wd <- getwd()
#  ZBOTDRE1 <- -1.15  # diepte sloot (ivm droogval)

#  limits for plotting
xbound <- as.Date(c("1997-01-01","2000-01-01"))
cex=0.7

# read sim
filnam    <- paste("./result.swb")
sim      <- read.csv(file = filnam, header = TRUE, as.is = TRUE, skip = 21) 
sim$Date <- as.Date(sim$Date)
sim$GWL  <-  0.01 * (sim$GWL)   # m 
sim$WLST <-  0.01 * (sim$WLST)  # m 
sim$WLS  <-  0.01 * (sim$WLS)   # m 
# sim$WLS  <- ifelse(sim$WLS >= ZBOTDRE1, sim$WLS, NA)
sim      <- subset(sim, Date>=xbound[1]  &  Date<=xbound[2] )
# read observed g
filnam    <- paste("./observed/groundwatlev_wg02.csv.txt")
obsg      <- read.csv(file = filnam, header = TRUE, as.is = TRUE, skip = 3) 
obsg$Date <- as.Date(obsg$Date,format="%Y-%b-%d")
obsg      <- subset(obsg, Date>=xbound[1]  &  Date<=xbound[2] )
obsg <- na.omit(obsg)
# read observed surfacwat
filnam   <- paste("./observed/surfwatlev_nearwg02.csv.txt")
obss      <- read.csv(file = filnam, header = TRUE, as.is = TRUE, skip = 3, na.strings = "NA")
obss$Date <- as.Date(obss$Date)  # ,format="%Y-%b-%d")
obss      <- subset(obss, Date>=xbound[1]  &  Date<=xbound[2] )


# generate plot 
png(file = "Wildenborch.png", width = 1024, height = 768, pointsize = 12, bg = "white")
# png(file = "Wildenborch.png", width = 2000, height = 1000, pointsize = 12, bg = "white")
# par(mfrow = c(1, 1), mar = c(bottom = 3, left = 3, top = 1, right = 1))
xmin <- min(sim$Date,obsg$Date,na.rm=TRUE)
xmax <- max(sim$Date,obsg$Date,na.rm=TRUE)
xlim <- c(xmin,xmax)
ymin <- min(sim$GWL,obsg$Gwl,sim$WLS,sim$WLST,na.rm=TRUE)
ymax <- max(sim$GWL,obsg$Gwl,sim$WLS,sim$WLST,na.rm=TRUE)
ylim <- c(ymin,ymax)
main <- paste("Levels (meter below the soil surface)")
pc = 19      # solid point
plot(GWL ~ Date, sim, type="l",frame.plot=FALSE, main=main, adj=0, xlab="",ylab="",
        xlim=xlim,ylim=ylim,cex=1.0,cex.axis=1.5,col="green")
xx <- c(sim$Date,rev(sim$Date))
yy <- c( rep(ymin,length(sim$WLST)) , rev(sim$WLST) )
polygon(x=xx, y=yy, col="skyblue", border = NA)
lines(GWL ~ Date, sim, col="green",lty="solid",lwd=2)
points(Gwl ~ Date, obsg, col="red",pch=pc, cex=0.5)
lines(WLS ~ Date, sim, col="blue4",lty="solid",lwd=2)
xpos <- as.Date("1999-08-15")
legend(x=xpos,y=ymax,legend=c("GWL_sim","GWL_obs","SWL_sim","Weir_crest"), ncol=1,
        lty=c("solid","blank","solid","blank"),pch=c(-1,pc,-1,22),pt.cex=c(1,1,1,2),
        pt.bg=("skyblue"), inset=-0.1, bg="white",
        col=c("green","red","blue","skyblue"),title = "LEGEND  ",cex=0.9)
# close plotfile
dev.off()

