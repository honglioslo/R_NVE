#' calculate daily amonoly from a zoo object
#'
#' This function calculate daily amonoly from a zoo object.
#' @param inZoo file to read.
#' @keywords data
#' @export
#' @examples
#' DailyAmon(inZoo = inZoo)

DailyAmon <- function(inZoo = inZoo) {
	if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
	if ( ! require(lubridate) )        { install.packages("lubridate");        library(lubridate) }
	AmonYear <- 2000
	Dates <- seq(as.Date(sprintf("%04d-01-01", AmonYear)), as.Date(sprintf("%04d-12-31", AmonYear)), by = "day")
	ncolInZoo <- dim(inZoo)[2]
	if (is.null(ncolInZoo)) ncolInZoo <- 1
	DailyAmon <- zoo(matrix(nrow = length(Dates), ncol = ncolInZoo), Dates)
	for (iM in seq(ncolInZoo)) {
	for (iD in seq(length(Dates))) {

		monN <- month(Dates[iD])
		dayN <- day(Dates[iD])
		calN <- intersect(which(month(inZoo) %in% monN), which(day(inZoo) %in% dayN))
		DailyAmon[iD, iM] <- mean(inZoo[calN, iM], na.rm = TRUE)
	}
	Feb29 <- which(index(DailyAmon) == "2000-02-29")
	if (is.na(DailyAmon[Feb29, iM])) {
		DailyAmon[Feb29, iM] <- mean(DailyAmon[Feb29-1, iM], DailyAmon[Feb29+1, iM])
	}
	}
	return(DailyAmon)
}
