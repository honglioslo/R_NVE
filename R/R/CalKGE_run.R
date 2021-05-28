#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
print(args)
PathSim <- args[1]
SimNum <- args[2]
PathObs <- args[3]
ObsNum <- args[4]
DateStart <- args[5]
DateEnd <- args[6]
W <- args[7]
if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
if ( ! require(hydroGOF) )      { install.packages("hydroGOF");      library(hydroGOF) }
if ( ! require(boot) )      { install.packages("boot");      library(boot) }
SimNum <- unlist(strsplit(SimNum, ","))
ObsNum <- unlist(strsplit(ObsNum, ","))
DateStart <- as.Date(DateStart)
DateEnd <- as.Date(DateEnd)
W <- as.numeric(unlist(strsplit(W, ",")))

nSta <- length(SimNum)
KGE_i <- vector(length = nSta)
NSE_i <- vector(length = nSta)
MAE <- vector(length = nSta)
pBias <- vector(length = nSta)
Pw_i <- vector(length = nSta)

for (iSta in seq(1, nSta, 1)) {
	print(c(iSta, SimNum[iSta], ObsNum[iSta]))

	FileObsRunoff <- sprintf("%s/%s", PathObs, ObsNum[iSta])
	print(FileObsRunoff)
	ObsRunoff <- read.table(FileObsRunoff, header = FALSE)
	ObsRunoff[which(ObsRunoff[,2] <= 0), 2] <- NA
	ObsDates <- as.Date(ObsRunoff[,1], format = "%Y%m%d/1200")
	ObsRunoffZoo <- zoo(ObsRunoff[,2], ObsDates)

	ModelRunoffZoo <- zoo(NA, index(ObsRunoffZoo));
	FileModelRunoff <- sprintf("%s/%s", PathSim, SimNum[iSta]) 
	print(FileModelRunoff)
	ModelRunoff <- read.table(FileModelRunoff, header = FALSE)
	ModelRunoff[which(ModelRunoff[,2] <= 0), 2] <- NA
	ModelDates <- as.Date(ModelRunoff[,1], format = "%Y%m%d/1200")
	ModelRunoff <- zoo(ModelRunoff[,2], ModelDates)

	Common <- as.Date(intersect(index(ModelRunoff), index(ModelRunoffZoo)), original = "1970-01-01")
	ModelRunoffZoo[match(Common, index(ModelRunoffZoo))]<- ModelRunoff[match(Common, index(ModelRunoff))]

	KGE_i[iSta] <- KGE(ModelRunoffZoo, ObsRunoffZoo, na.rm = TRUE)
	NSE_i[iSta] <- NSE(ModelRunoffZoo, ObsRunoffZoo, na.rm = TRUE)
	MAE[iSta] <- mae(ModelRunoffZoo, ObsRunoffZoo, na.rm = TRUE)
	pBias[iSta] <- pbias(ModelRunoffZoo, ObsRunoffZoo, na.rm = TRUE)
	
	Pw_i[iSta] <- sum(!is.na(ModelRunoffZoo))
}
V <- which(!is.na(KGE_i))
SumW <- W[V] * Pw_i[V]
KGE_f <- sum(KGE_i[V] * SumW)/sum(SumW) 
KGE_re <- data.frame(Obs = ObsNum, Sim = SimNum, KGE = KGE_i, NSE = NSE_i, MAE = MAE, pbias = pBias)
write.table(KGE_re, file = sprintf("%s/run_obj_all.txt", PathSim), quote = FALSE, row.names = FALSE, col.names = TRUE, sep = ";")
write.table(KGE_f, file = sprintf("%s/run_obj.txt", PathSim), quote = FALSE, row.names = FALSE, col.names = FALSE)
print(" all runoff KGE ")
print(KGE_re)
print(sprintf("final runoff KGE: %.5f", KGE_f))