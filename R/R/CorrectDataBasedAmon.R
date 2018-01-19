#' CorrectDataBasedAmon
#'
#' This function calculate daily amonoly from a zoo object.
#' @param inZoo input time series.
#' @param inAmon input anomoly.
#' @keywords data
#' @export
#' @examples
#' CorrectDataBasedAmon()

# CorrectDataBasedAmon <- function(inZoo = inZoo, inAmon = inAmon) {
	# if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
	# if ( ! require(lubridate) )        { install.packages("lubridate");        library(lubridate) }
	
	# out <- inZoo
	# #print(length(out))
	# Mloc <- which(is.na(inZoo))
	# if (length(Mloc) == 0) return(out)
	# for (iM in seq(length(Mloc))) {
		# monN <- month(inZoo[Mloc[iM]])
		# dayN <- day(inZoo[Mloc[iM]])
		# calN <- intersect(which(month(inAmon) %in% monN), which(day(inAmon) %in% dayN))
		# out[Mloc[iM]] <- inAmon[calN]
	# }
	# if (length(which(is.na(out))) > 0) stop("still missing value in out")
	# return(out)
# }


CorrectDataBasedAmon <- function(inZoo = inZoo, inAmon = inAmon) {
	if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
	if ( ! require(lubridate) )        { install.packages("lubridate");        library(lubridate) }
	datesIn  <- index(inZoo)
	datesOut <- seq(datesIn[1], datesIn[length(datesIn)], by = "day")
	
	out <- cbind(datesIn, as.data.frame(inZoo))
	names(out)[1] <- "index"
	out$index <- as.character(out$index)
	# find the missing and NA values in the input
	#print(length(out))

	Miloc <- which(!datesOut %in% datesIn)
	if (length(Miloc) != 0) {
		for (iM in Miloc) {
			out <- add_row(out, index = NA, .before = iM)
			out[iM,1] <- as.character(datesOut[iM]) 
		}	
	}
	Nrow <- dim(out)[1]
	Ncol <- dim(out)[2]
	
	Naloc <- which(is.na(out))
	Nuloc <- which(is.null(out))
	Nloc <- c(Naloc, Nuloc)
	
	if (length(c(Naloc, Nuloc)) == 0) return(out)
	inAmonMon <- month(index(inAmon))
	inAmonDay <- day(index(inAmon))
	inAmonD <- as.data.frame(inAmon)
	for (iM in seq(length(Nloc))) {
		irow <- Nloc[iM] %% Nrow
		if (irow == 0) irow <- Nrow
		icol <- (Nloc[iM] - irow)/Nrow + 1
		monN <- month(out[irow, 1])
		dayN <- day(out[irow, 1])
		calN <- intersect(which(inAmonMon %in% monN), which(inAmonDay %in% dayN))
		out[irow, icol] <- inAmonD[calN, icol-1]
	}
	if (length(which(is.na(out))) > 0) stop("still missing value in out")
	return(out)
}

