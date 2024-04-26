## ----
# r-procedure to plot VanGenuchten Mualem parameters 
#
## load packages

# output-switches   on/off
floutput_tables = "on"   
floutput_graphs = "on"

# set work directory
# setwd("d:\\Kroes\\swap4\\Report2780_Swap4_Wofost_Hydrology\\InstallPackageSwap40\\xdata\\soils\\bofek2012\\MVG\\")
# read data from table
filnam = "staringreeks1994.csv"
tmps <- read.table(file = filnam, header = TRUE, as.is = TRUE, skip = 0, sep = ",")

plotfiletype<-"jpg"
y<-c(10.0,-500.0)
textfact = 0.8
#head <- seq(from = -100000, to = -0.1, by = 0.1)
head <- c(-1e7,
      -5000000., -2000000., -1500000., -1200000., -1000000., 
       -500000.,  -200000.,  -150000.,  -120000.,  -100000., 
        -50000.,   -20000.,   -15000.,   -12000.    -10000.,
         -5000.,    -2000.,    -1500.,    -1200.,    -1000., 
          -500.,     -200.,     -150.,     -120., 
          seq(from = -100, to = -10,  by = 5),
          seq(from = -9.5, to = -1.0, by = 0.5),
          seq(from = -0.9, to =  0.0, by = 0.1))
           #,seq(from = 0.1, to = 1.0, by = 0.1))

# Van Genuchten's function theta(head) and  K(head) for all layers i
rangex1 <- c(0,0.9)  # theta
rangey1 <- c(-1,7)   # log10(-head)
rangex2 <- c(-1,7)   # log10(-head)
rangey2 <- c(-7,3)   # log10(K)
for (irec in 1:nrow(tmps))   {
    #   theta(head)
    help1 <- (abs(tmps$ALFAD[irec] * head))**tmps$NPAR[irec]
    help2 <- (1 + help1)**(1-1/tmps$NPAR[irec])
    theta = tmps$ORES[irec] + (tmps$OSAT[irec] - tmps$ORES[irec]) / help2

    if (floutput_graphs == "on")  {
      par(mfrow = c(2, 1),mar = c(bottom = 5, left = 5, top = 1, right = 2))
      plot(x=theta, y=log10(-head), type="l", lty = 1, 
        xlab = expression(italic(theta)), ylab = expression(log[10](-italic(h))),
        main = tmps$comment[irec], xlim = range(rangex1),ylim = range(rangey1))
    }

    #   K(head)
    SATrel <- (theta-tmps$ORES[irec]) / (tmps$OSAT[irec]-tmps$ORES[irec])
    m <- 1- 1/tmps$NPAR[irec]
    help3 <- 1-(1-SATrel**(1/m))**m
    K = tmps$KSAT[irec] * SATrel**tmps$LEXP[irec] * help3**2

    if (floutput_graphs == "on")  {
      plot(x = log10(-head), y = log10(K), type="l", lty = 1, 
         xlab = expression(log[10](-italic(h))), ylab = expression(log[10](italic(K))),
         main = tmps$comment[irec], xlim = range(rangex2),ylim = range(rangey2))
      plotfile <- paste("./graphs/",tmps$comment[irec],".jpg",sep="")
      savePlot(filename=plotfile, type=c(plotfiletype))
    }

    if (floutput_tables == "on")  {
      SPUtable <- data.frame(
        headtab = head,
        thetatab = theta,
        conductab = K)
      outfile <- paste("./tables/",tmps$comment[irec],".csv",sep="")
      SPUtable <- as.data.frame(lapply(X=SPUtable,FUN=formatC, format="e", digits=6))
      write.table(x=SPUtable, file = outfile, append = FALSE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", row.names = FALSE,
            col.names = TRUE)
    }
}
