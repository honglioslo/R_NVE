#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)
print("Correction Runoff due to change in Area")
print(args)
PathSim <- args[1]
print("in put")
print(sprintf("PathSim: %s", PathSim))
PCat <- "00422100"
DCat <- c("00422000", "00420000", "00400000")

PRunoff <- sprintf("%s/beforeCorrection/dew_%s.var", PathSim, PCat) 
print(PRunoff)
PRunoff <- read.table(PRunoff, header = FALSE)
Dates <- as.Date(PRunoff[,1], format = "%Y%m%d/1200")
CorrDate <- which(Dates < as.Date("1980-01-01"))


nSta <- length(DCat)
for (iSta in seq(1, nSta, 1)) {
	DRunoff <- sprintf("%s/beforeCorrection/dew_%s.var", PathSim, DCat[iSta]) 
	print(DRunoff)
	DRunoff <- read.table(DRunoff, header = FALSE)
	DRunoff[CorrDate,2] <- DRunoff[CorrDate,2] - PRunoff[CorrDate,2]
	if(sum(DRunoff[CorrDate,2] <0) >0) {
		print(sprintf("runoff at %s is less that %s", DCat[iSta], PCat))
	}
	print(sprintf("%s/dew_%s.var", PathSim, DCat[iSta]))
	write.table(DRunoff, file = sprintf("%s/dew_%s.var", PathSim, DCat[iSta]), col.names = FALSE, row.names = FALSE, quote = FALSE, dec = ".", sep = "    ")

}
