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
#CalKGE_run <- function(PathSim, SimNum, PathObs, ObsNum, DateStart, DateEnd, W) {
if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
if ( ! require(hydroGOF) )      { install.packages("hydroGOF");      library(hydroGOF) }
if ( ! require(boot) )      { install.packages("boot");      library(boot) }
print("calculate runoff criteria")
## this function is only desinged for evo_glac project; runoff
## there are seven discharge stations and one resevoir
print("in put")
print(sprintf("PathSim: %s", PathSim))
print(sprintf("SimNum: %s", SimNum))
print(sprintf("PathObs: %s", PathObs))
print(sprintf("ObsNum: %s", ObsNum))
print(sprintf("DateStart: %s", DateStart))
print(sprintf("DateEnd: %s", DateEnd))
print(sprintf("Weighting: %s", W))	
SimNum <- unlist(strsplit(SimNum, ","))
ObsNum <- unlist(strsplit(ObsNum, ","))
DateStart <- as.Date(DateStart)
DateEnd <- as.Date(DateEnd)
W <- as.numeric(unlist(strsplit(W, ",")))
nSta <- length(SimNum)

rmse_i <- vector(length = nSta)
Pw_i <- vector(length = nSta)


# is
# glacier mass balance, summer and winter seperate, specific dates

print("start ice")


nSta <- length(ObsNum)
for (iSta in seq(1, nSta, 1)) {
  FileModelMass <- sprintf("%s/%s", PathSim, SimNum[iSta])
  print(FileModelMass)
  ModelMass <- read.table(FileModelMass, header = FALSE)
  ModelDates <- as.Date(ModelMass[, 1], format = "%Y%m%d/1200")
  ModelMassZoo <- zoo(ModelMass[, 2], ModelDates)
  ModelMassZoo <-
    ModelMassZoo[which(index(ModelMassZoo) >= DateStart &
                         index(ModelMassZoo) <= DateEnd)]
  if (length(which(ModelMassZoo == -9999)) > 0) {
    print("simulation result have -9999 missing value ")
    rmse_i[iSta] <- 1000000 
    Pw_i[iSta] <- 1000
    next 
  }
  
  MassObsFile <-
    sprintf("%s/%s/massbalanceData_%s.csv", PathObs, ObsNum[iSta], ObsNum[iSta])
  MassObsData <-
    read.table(MassObsFile,
               header = TRUE,
               colClasses = "character",
               sep = ";")
  hMinPrevYear <-
    as.Date(MassObsData$DateMinPrevYear, format = "%d.%m.%Y")
  hMin <- as.Date(MassObsData$DateMin, format = "%d.%m.%Y")
  hMax <- as.Date(MassObsData$DateMax, format = "%d.%m.%Y")
  
  UsedObs <- which(hMinPrevYear >= DateStart & hMin <= DateEnd)
  ObsMass <- vector(length = length(UsedObs))
  SimMass <- vector(length = length(UsedObs))
  for (iUse in 1:length(UsedObs)) {
    #print(MassObsData[UsedObs[iUse], c(1, 14)])
    ObsMass[iUse] <-
      as.numeric(MassObsData$Ba[UsedObs[iUse]]) * 1000
    
    SimMass[iUse] <-
      ModelMassZoo[which(index(ModelMassZoo) == hMin[UsedObs[iUse]])]
    - ModelMassZoo[which(index(ModelMassZoo) == hMinPrevYear[UsedObs[iUse]])]
    
  }
   rmse_i[iSta] <- rmse(SimMass, ObsMass, na.rm = TRUE)
   Pw_i[iSta] <- length(!is.na(ObsMass))
}
V <- which(!is.na(rmse_i))
SumW <- W[V] * Pw_i[V]
RMSE_f <- sum(rmse_i[V] * SumW)/sum(SumW) 
rmse_re <- data.frame(Obs = ObsNum, rmse = rmse_i)
write.table(rmse_re, file = sprintf("%s/is_obj_all.txt", PathSim), quote = FALSE, row.names = FALSE, col.names = TRUE, sep = ";")
write.table(RMSE_f, file = sprintf("%s/is_obj.txt", PathSim), quote = FALSE, row.names = FALSE, col.names = FALSE)
print(" final is rmse")
print(RMSE_f)
print("all is rmse")
print(rmse_re)
