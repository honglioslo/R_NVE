#' writeHBV_metinput.R
#'
#' This function write the HBV format input met data.
#' @param DataZoo data to write
#' @param fileName file to write.
#'
#' @keywords data
#' @export
#' @examples
#' writeHBV_metinput()

writeHBV_metinput <- function(DataZoo, fileName = "data/input_data.txt") {
	if ( ! require(zoo) )  { install.packages("zoo");        library(zoo) }
	print(fileName)
	write.table(t(c("Time", rep("pre", length(pre_files)), rep("tem", length(tem_files)))),
	            file = fileName, append = FALSE, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
	write.table(round(DataZoo, digits = 3),
	  file = fileName, col.names = TRUE, row.names = TRUE, append = TRUE, quote = FALSE, sep = "\t")

}
