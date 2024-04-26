# set work directory
#  setwd("d:\\Kroes\\ProjectenKleine2015\\EureyeOpener_AnnaPauwlowna\\Sim_AP\\CaseS\\1.referentie\\work")
# ---------------------------   main program  --------------------------------------------------------
# load packages
library(lattice)
library(RColorBrewer)

# names of files with simulated results
filnamsim <- c("./result.vap")

# switch to plot  q_vert, theta, concentrations, temperature, 
swplt_qvert     <- 0
swplt_head      <- 1
swplt_theta     <- 1
swplt_conc      <- 1
swplt_conc_log  <- 0
swplt_temp      <- 0
DepthBottom     <-   -200.0  # Depth (cm) of bottom of compartments used for contour graph 
YearMin <- 2012
YearMax <- 2015

# read water and solute
vap <- read.csv(file = filnamsim[1], header = TRUE, as.is = TRUE, skip = 11) 
vap <- subset(vap,(depth>DepthBottom))
vap <- subset(vap,!is.na(vap$wcontent))
vap$date <- as.Date(vap$date)
vap$year = 1900 + as.POSIXlt(vap$date)$year
vap 	 <- subset(vap,(year>YearMin & year<YearMax))
x <- vap$dcum   # time in case of a contour plot as function of time
z <- vap$depth

# set graphical parameters
#color ramp
col.l <- colorRampPalette(c('red', 'orange', 'yellow', 'green', 'cyan', 'blue'))
col.2 <- colorRampPalette(c('blue', 'cyan', 'yellow', 'green', 'orange', 'red'))
col.3 <- colorRampPalette(c('blue', 'yellow', 'red'))
col.4 <- colorRampPalette(c('red', 'orange', 'yellow', 'green', 'cyan', 'blue', 'purple'))

# plot water fluxes q
if (swplt_qvert==1) {
   textmain <- paste("Water fluxes qv [cm/d]; positive=upward")
   values <- vap$waterflux
   dat <- data.frame(x = x, z = z, v = values)
   sr  <- summary(dat$v)
   sr[7] <- 0
#   names(sr[7]) <- paste("zero")
   sr <-sort(sr)
   png(file = "ResultSwapWaterfluxesContour.png", width = 1024, height = 768, pointsize = 16, bg = "white")
   print(levelplot(
       v ~ x * z, data = dat,
       col.regions=col.2, 
   	   at = sr, 
       xlab = "Time (days)",
       ylab = "Depth (m)",
       main = textmain
   ))
   dev.off()
}

# plot pressure head
if (swplt_head==1) {
   textmain <- paste("Soil Pressure Head [cm]; unsaturated: log10(-h), saturated: -log10(h)")
   valuesp <- vap$phead
   datx <- valuesp
   for (i in (1:length(valuesp))) {
		if(valuesp[i]<0)  { datx[i] <- log10(-1*valuesp[i]) }
		if(valuesp[i]==0) { datx[i] <- 0.0 }
		if(valuesp[i]>0)  { datx[i] <- -1*log10(valuesp[i]) }
   }
   dat <- data.frame(x = x, z = z, v = datx)
   vmin <- min(dat$v)
   vmax <- max(dat$v)
   vstep <- (vmax-vmin)/100
   png(file = "ResultSwapPressHeadContour.png", width = 1024, height = 768, pointsize = 16, bg = "white")
   print(levelplot(
       v ~ x * z, data = dat,
       col.regions=col.2, 
   	   at = seq(from=vmin,to=vmax,by=vstep), 
       xlab = "Time (days)",
       ylab = "Depth (m)",
       main = textmain
   	))
   dev.off()
}

# plot water content 
if (swplt_theta==1) {
   textmain <- paste("Water Content [-]")
   valuesw <- vap$wcontent         # cm3/cm3 of  dm3/dm3
   dat <- data.frame(x = x, z = z, v = valuesw)
   vmin  <- min(dat$v)
   vmax  <- max(dat$v)
   vstep <- (vmax-vmin)/100
   png(file = "ResultSwapThetaContour.png", width = 1024, height = 768, pointsize = 16, bg = "white")
   print(levelplot(
       v ~ x * z, data = dat,
       col.regions=col.l, 
   	   at = seq(from=vmin,to=vmax,by=vstep), 
       xlab = "Time (days)",
       ylab = "Depth (m)",
       main = textmain
   ))
   dev.off()
}

# plot concentrations 
if (swplt_conc==1) {
   textmain <- paste("Solute Concentration [mg/l]")
   valuess  <- vap$solute1*1000     # mg/cm3 * 1000 wordt  mg/l of mg/dm3
   dat      <- data.frame(x = x, z = z, v = valuess)
   vmin  <- min(dat$v)
   vmax  <- max(dat$v)
#   vmin  <- 0
#   vmax  <- 8000
   vstep <- (vmax-vmin)/100
   png(file = "ResultSwapSoluteConcContour.png", width = 1024, height = 768, pointsize = 16, bg = "white")
   print(levelplot(
       v ~ x * z, data = dat,
       col.regions=col.3, 
   	   at = seq(from=vmin,to=vmax,by=vstep), 
       xlab = "Time (days)",
       ylab = "Depth (m)",
       main = textmain
   	))
   dev.off()
}

# plot concentrations as log-values
if (swplt_conc_log==1) {
   textmain <- paste("Solute Concentration, log(c) in [mg/l]")
   valuess  <- vap$solute1*1000     # mg/cm3 * 1000 wordt  mg/l of mg/dm3
   datx <- valuess
   for (i in (1:length(valuesp))) {
		if(valuess[i]<0)  { datx[i] <- -1*log10(-1*valuess[i]) }    # neg concentrations must not occur!!
		if(valuess[i]==0) { datx[i] <- 0.0 }
		if(valuess[i]>0)  { datx[i] <- log10(valuess[i]) }
   }
   dat      <- data.frame(x = x, z = z, v = datx)
   vmin  <- min(dat$v)
   vmax  <- max(dat$v)
#   vmin  <- 0
#   vmax  <- 8000
   vstep <- (vmax-vmin)/100
   png(file = "ResultSwapSoluteLogConcContour.png", width = 1024, height = 768, pointsize = 16, bg = "white")
   print(levelplot(
       v ~ x * z, data = dat,
       col.regions=col.3, 
   	   at = seq(from=vmin,to=vmax,by=vstep), 
       xlab = "Time (days)",
       ylab = "Depth (m)",
       main = textmain
   	))
   dev.off()
}

# plot soil temperature
if (swplt_temp==1) {
   textmain <- paste("Soil temperatures [C]")
   valuest <- vap$temp            # deg C
   dat <- data.frame(x = x, z = z, v = valuest)
   vmin  <- min(dat$v)
   vmax  <- max(dat$v)
   vstep <- (vmax-vmin)/100
   png(file = "ResultSwapSoilTemperatureContour.png", width = 1024, height = 768, pointsize = 16, bg = "white")
   print(levelplot(
       v ~ x * z, data = dat,
       col.regions=col.2, 
   	   at = seq(from=vmin,to=vmax,by=vstep), 
       xlab = "Time (days)",
       ylab = "Depth (m)",
       main = textmain
   ))
   dev.off()
}
# ---------------------------   end of main program  --------------------------------------------------------
