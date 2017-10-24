#' read nve data
#'
#' This function read the HBV format data and return a zoo object.
#' @param fileName file to read.
#' @param na.rm if remove the null values, "NULL" not include, "NA" eller "any"
#' @keywords data
#' @export
#' @examples
#' readHBV(fileName = "data/dew_00400000.out", na = "NULL")

readHBV <- function(fileName, na) {
	if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
	print(fileName)
	ObsRunoff <- read.table(fileName, header = FALSE)
	ObsRunoff[which(ObsRunoff[,2] == -9999),2] <- NA
	ObsDates <- as.Date(ObsRunoff[,1], format = "%Y%m%d/1200")
	ObsRunoffZoo <- zoo(ObsRunoff[,2], ObsDates)
	if (na == "NULL") {
		ObsRunoffZoo <- ObsRunoffZoo[-which(is.na(ObsRunoffZoo))]
	} else if (na == "NA") {
		ObsRunoffZoo <- ObsRunoffZoo
	} else {
		ObsRunoffZoo[which(is.na(ObsRunoffZoo))] <- na
	}
	return(ObsRunoffZoo)
}
