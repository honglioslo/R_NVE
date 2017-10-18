#' generate several pst files with format and sepecific distribution
#'
#'
#' @param infile file to read.
#' @param outfile file to write.
#' @param nout number of out file to write.
#' @param dist distribution of values to follow, at prsent at only use runif.

#' @keywords data
#' @export
#' @examples
#' random_pst()

random_pst <- function(infile = "data/dew.pst", outfile = "data/dew.pst_r", nout = 3, dist = "runif") {
  library(readtext)
  intext <- readtext(infile, text_field = NULL)

  posStart = gregexpr('* parameter data', intext$text)
  posEnd = gregexpr('* observation groups', intext$text)

  paraText <- substr(intext$text, posStart[[1]][1] + nchar('* parameter data'), posEnd[[1]][1]-3)
  paraText <- unlist(strsplit(paraText, '[\n]'))
  paraText <- unlist(strsplit(paraText, ' '))
  paraText <- paraText[-which(paraText=="")]
  paraText <- matrix(paraText, ncol = 10, byrow = TRUE)

  minValue <- as.numeric(paraText[,5])
  maxValue <- as.numeric(paraText[,6])
  fa <- 1000/rowMeans(cbind(minValue, maxValue))
  intValue <-matrix(ncol = nout, nrow = dim(paraText)[1])
  for (iPar in seq(dim(paraText)[1])) {
    intValue[iPar,] <- runif(nout, min = minValue[iPar]*fa[iPar], max = maxValue[iPar]*fa[iPar])/fa[iPar]
  }

  for (iPst in seq(nout)) {
    paraTextNew <- paraText
    paraTextNew[,4] <- sprintf("%11.6e", intValue[,iPst])
    NChar <- max(nchar(paraTextNew)) + 5
    Finaltext <- NULL
    for (iPar in seq(dim(paraText)[1])) {
      str <- paraTextNew[iPar,]
      runString <- "text <- paste(sprintf('%"
      runString <- sprintf("%s%ds', str), sep = '', collapse = '')", runString, NChar)
      eval(parse(text=runString))
      Finaltext <- c(Finaltext, text)
    }
    FinaltextNew <- paste(Finaltext, sep = '', collapse = '\n')
    writeText <- paste(substr(intext$text, 1, posStart[[1]][1] + nchar('* parameter data') - 1), FinaltextNew, substr(intext$text, posEnd[[1]][1]-2, nchar(intext$text)), sep = "")
    writeStringA <- "write(writeText, sprintf('%s%0"
    writeStringB <- "d', outfile, iPst))"
    writeString <- sprintf("%s%d%s", writeStringA, nchar(nout) + 1, writeStringB)
    eval(parse(text=writeString))
  }
  print("done")
}
