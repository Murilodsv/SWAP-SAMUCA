# set work directory
# setwd("D:\\Kroes\\swap4\\Report2780_Swap4_Wofost_Hydrology\\InstallPackageSwap40\\cases\\4.OxygenStress")

## load packages

ymax_TStressPerc <- 20

# open create and close plotfile
png(file = "ResultStressYr.png", width = 1024, height = 768, pointsize = 16, bg = "white")
par(mfrow = c(3, 1),mar = c(bottom = 4, left = 4, top = 3, right = 4))
# read stress results
stFiles <- list.files(path = "./", pattern = "result.str", full.names = FALSE)
for (ist in (1:length(stFiles))) {
#	ist <- 1
	st <- read.csv(stFiles[ist], header = TRUE, as.is = TRUE, skip = 6) 
	st$year <- as.POSIXlt(as.Date(st$Date))$year+1900
	yrs     <- unique(st$year)
	nyrs <- length(unique(st$year))
	tmp<- matrix(data = NA, nrow = nyrs, ncol = 13, byrow = FALSE, dimnames = NULL) 
	statsyrset <- as.data.frame(tmp) 
	names(statsyrset) <- paste(c("scen","year","Tpot","Tact","Tredtot","Tredwet","Treddry","Tredsol","Tredfrs",
							 "Twetperc","Tdryperc","Tsolperc","Tfrsperc")) 
	for (iyr in (1:nyrs)) {
#		iyr <- 1
		tmpst <- subset (st, yrs[iyr]==st$year)
		statsyrset$myear[iyr] <- mean(tmpst$year)
		statsyrset$Tpot[iyr] <- sum(tmpst$Tpot)*10   # from cm to mm
		statsyrset$Tact[iyr] <- sum(tmpst$Tact)*10   # from cm to mm
		statsyrset$Tredwet[iyr] <- sum(tmpst$Tredwet)*10   # from cm to mm
		statsyrset$Treddry[iyr] <- sum(tmpst$Treddry)*10   # from cm to mm
		statsyrset$Tredsol[iyr] <- sum(tmpst$Tredsol)*10   # from cm to mm
		statsyrset$Tredfrs[iyr] <- sum(tmpst$Tredfrs)*10   # from cm to mm
		statsyrset$Tredtot[iyr] <- statsyrset$Tredfrs[iyr] + statsyrset$Tredsol[iyr] +
								   statsyrset$Treddry[iyr] + statsyrset$Tredwet[iyr]
		statsyrset$Twetperc[iyr] <- 100*statsyrset$Tredwet[iyr]/statsyrset$Tpot[iyr]
		statsyrset$Tdryperc[iyr] <- 100*statsyrset$Treddry[iyr]/statsyrset$Tpot[iyr]
		statsyrset$Tsolperc[iyr] <- 100*statsyrset$Tredsol[iyr]/statsyrset$Tpot[iyr]
		statsyrset$Tfrsperc[iyr] <- 100*statsyrset$Tredfrs[iyr]/statsyrset$Tpot[iyr]
	}
	statsyrset$scen <- stFiles[ist]
	# store result
	fileres   <- paste("variant_",ist,".per.csv",sep="")
	write.csv(x= statsyrset, file = fileres, quote = FALSE, row.names = FALSE)

	# plot per year
	ylim <- c(0,ymax_TStressPerc)
	plot(Twetperc ~ myear, statsyrset, ylim=ylim,main="Oxygen stress (%) Tpot-Tact",type="b")
	plot(Tdryperc ~ myear, statsyrset, ylim=ylim,main="Drought stress (%) Tpot-Tact",type="b")
	plot(Tsolperc ~ myear, statsyrset, ylim=ylim,main="Salinity stress (%) Tpot-Tact",type="b")

}
dev.off()

#  plot against time
sts <- read.csv(stFiles[ist], header = TRUE, as.is = TRUE, skip = 6) 
sts$Date      <- as.Date(sts$Date)
sts$Tp_Ta <- sts$Tpot - sts$Tact
png(file = "ResultStressDT.png", width = 1024, height = 768, pointsize = 16, bg = "white")
par(mfrow = c(4, 2),mar = c(bottom = 4, left = 4, top = 3, right = 4))
for (i in c(7,8,9,ncol(sts),10,11,12,13)) {
	if(!is.na(sts[1,i])) {
		plot(x=sts$Date,y=(10*sts[,i]),type="l",main=paste(names(sts)[i],"(mm per day)"), ylab="")
	}
}
dev.off()

