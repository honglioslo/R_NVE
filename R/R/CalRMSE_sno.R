args = commandArgs(trailingOnly=TRUE)
print(args)
PathSim <- args[1]
SimNum <- args[2]
PathObs <- args[3]
ObsNum <- args[4]
DateStart <- args[5]
DateEnd <- args[6]
if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
if ( ! require(hydroGOF) )      { install.packages("hydroGOF");      library(hydroGOF) }
if ( ! require(boot) )      { install.packages("boot");      library(boot) }
SimNum <- unlist(strsplit(SimNum, ","))
ObsNum <- unlist(strsplit(ObsNum, ","))
DateStart <- as.Date(DateStart)
DateEnd <- as.Date(DateEnd)

nSta <- length(SimNum)
rmse_i <- vector(length = nSta)
Pw_i <- vector(length = nSta)

for (iSta in seq(1, nSta, 1)) {
	print(c(iSta, SimNum[iSta]))
	print(ObsNum[iSta])
	FileObsRunoff <- sprintf("%s/%s", PathObs, ObsNum[iSta])
	print(FileObsRunoff)
	ObsRunoff <- read.table(FileObsRunoff, header = FALSE)
	ObsRunoff[which(ObsRunoff[,2] <= 0), 2] <- NA
	ObsDates <- as.Date(ObsRunoff[,1], format = "%Y%m%d/1200")
	ObsRunoffZoo <- zoo(ObsRunoff[,2], ObsDates)
	ObsRunoffZoo <- ObsRunoffZoo * 1000 # change unit from m to mm

	ModelRunoffZoo <- zoo(NA, index(ObsRunoffZoo))
	FileModelRunoff <- sprintf("%s/%s", PathSim, SimNum[iSta]) 
	print(FileModelRunoff)
	ModelRunoff <- read.table(FileModelRunoff, header = FALSE)
	ModelRunoff[which(ModelRunoff[,2] < 0), 2] <- NA
	ModelDates <- as.Date(ModelRunoff[,1], format = "%Y%m%d/1200")
	ModelRunoff <- zoo(ModelRunoff[,2], ModelDates)

	Common <- as.Date(intersect(index(ModelRunoff), index(ModelRunoffZoo)), original = "1990-01-01")
	ModelRunoffZoo[match(Common, index(ModelRunoffZoo))]<- ModelRunoff[match(Common, index(ModelRunoff))]
	rmse_i[iSta] <- rmse(ModelRunoffZoo, ObsRunoffZoo, na.rm = TRUE)
	Pw_i[iSta] <- sum(!is.na(ModelRunoffZoo))

}
V <- which(!is.na(rmse_i))
SumW <- Pw_i[V]
RMSE_f <- sum(rmse_i[V] * SumW)/sum(SumW) 
rmse_re <- data.frame(Obs = ObsNum, rmse = rmse_i)
write.table(rmse_re, file = sprintf("%s/sno_obj_all.txt", PathSim), quote = FALSE, row.names = FALSE, col.names = TRUE, sep = ";")
write.table(RMSE_f, file = sprintf("%s/sno_obj.txt", PathSim), quote = FALSE, row.names = FALSE, col.names = FALSE)
print(" final snow rmse")
print(RMSE_f)
print("all snow rmse")
print(rmse_re)


