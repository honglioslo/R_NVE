#' calculate daily amonoly from a zoo object
#'
#' This function calculate daily amonoly from a zoo object.
#' @param inZoo file to read.
#' @keywords data
#' @export
#' @examples
#' DailyAmon()

DailyAmon <- function(inZoo = inZoo) {
	if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
	AmonYear <- median(unique(year(temp_zoo)))
	Dates <- seq(as.Date(sprintf("%04d-01-01", AmonYear)), as.Date(sprintf("%04d-12-31", AmonYear)), by = "day")
	DailyAmon <- zoo(vector(length = length(Dates)), Dates)
	for (iD in seq(length(Dates))) {
		monN <- month(Dates[iD])
		dayN <- day(Dates[iD])
		calN <- intersect(which(month(inZoo) %in% monN), which(day(inZoo) %in% dayN))
		DailyAmon[iD] <- mean(inZoo[calN], na.rm = TRUE)

	}
	return(DailyAmon)



}
