#' writeHBV_disc.R
#'
#' This function write the HBV format discharge data.
#' @param fileName file to write.

#' @keywords data
#' @export
#' @examples
#' writeHBV_disc(DataZoo, fileName =  "data/discharge_data.txt")

writeHBV_disc <- function(DataZoo, fileName) {
	if ( ! require(zoo) )        { install.packages("zoo");        library(zoo) }
	print(fileName)
	write.table(DataZoo, file = fileName, col.names = FALSE, row.names = TRUE, quote = FALSE, sep = "\t")
}
