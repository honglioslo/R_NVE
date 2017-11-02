#' CorrectDataBasedAmon
#'
#' This function calculate daily amonoly from a zoo object.
#' @param inZoo input time series.
#' @param inAmon input anomoly.
#' @keywords data
#' @export
#' @examples
#' CorrectDataBasedAmon()

CorrectDataBasedAmon <- function(inZoo = inZoo, inAmon = inAmon) {
	if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
	out <- inZoo
	#print(length(out))
	Mloc <- which(is.na(inZoo))
	if (length(Mloc) == 0) return(out)
	for (iM in seq(length(Mloc))) {
		monN <- month(inZoo[iM])
		dayN <- day(inZoo[iM])
		calN <- intersect(which(month(inAmon) %in% monN), which(day(inAmon) %in% dayN))
		out[Mloc[iM]] <- inAmon[calN]
	}
	if (length(which(is.na(out))) > 0) stop("still missing value in out")
	return(out)
}

