readHBV <- function(fileName) {
	print(fileName)
	ObsRunoff <- read.table(fileName, header = FALSE)
	ObsRunoff[which(ObsRunoff[,2] == -9999),2] <- NA
	ObsDates <- as.Date(ObsRunoff[,1], format = "%Y%m%d/1200")
	ObsRunoffZoo <- zoo(ObsRunoff[,2], ObsDates)

}
