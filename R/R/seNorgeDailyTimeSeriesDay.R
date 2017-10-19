#' get SeNorge time series for a point (x, y)
#'
#' @param x x direction coordinate lon, utm_e
#' @param y y direction coordinate lat, utm_n
#' @param s start date
#' @param e end date
#' @param var variable to extract: tm or rr
#' @param path path to data
#'
#' @keywords data
#' @export
#' @examples
#' seNorgeDailyTimeSeriesDay()

seNorgeDailyTimeSeriesDay <- function(x, y, s = s, e = e, var = "tm", path = "//hdata/grid/metdata/met_obs_v2.0") {
  if (! require("ncdf4")) {install.packages("ncdf4"); library(ncdf4)}
  if (! require("lubridate")) {install.packages("lubridate"); library(lubridate)}
  if (length(x) != length(y)) stop("x and y are not at the same length")
  reMatrx <- NULL
  if (var == "tm") VarN <- c("TEMP1d", "mean_temperature")
  if (var == "rr") VarN <- c("PREC1d", "precipitation_amount")
  s <- as.Date(s)
  e <- as.Date(e)

  np <- length(x)

  nc <- nc_open(sprintf("%s/%s/%04d/%s_%04d_%02d_%02d.nc", path, var, year(s), var, year(s), month(s), day(s)),readunlim=F)

  sprintf("%s/%s/%04d/%s_%s.nc", path, var, year(s), format(s, "%Y_%mm_%dd"))

  X <- ncvar_get(nc, "X")
  Y <- ncvar_get(nc, "Y")
  nc_close(nc)
  xx <- matrix(rep(X, length(Y)), nrow = length(Y))
  yy <- matrix(rep(Y, length(X)), ncol = length(X))
  nx <- vector(length = np)
  ny <- vector(length = np)
  for (ip in seq(np)) {
	dis <- (xx-x[ip])^2 + (yy-y[ip])^2
	nearest <- which.min(dis)
	nx[ip] <- which(X == xx[nearest])
	ny[ip] <- which(Y == yy[nearest])
  }
  minX <- min(nx)
  minY <- min(ny)
  maxX <- max(nx)
  maxY <- max(ny)
  nx <- nx - minX + 1
  ny <- ny - minY + 1
  for (iY in seq(s, e, by = "day")) {

  	nc <- nc_open(sprintf("%s/%s/%04d/%s_%04d_%02d_%02d.nc", path, var, year(iY), var, year(iY), month(iY), day(iY)),readunlim=F)
    ndays <- length(ncvar_get(nc, "time"))
	  seNorgeData <- ncvar_get(nc, VarN[2], start=c(minX,minY,1), count=c(maxX-minX+1,maxY-minY+1,ndays))
	#get <- seNorgeData[nx, ny, ]
	  get <- seNorgeData
    reMatrx <- c(reMatrx, get)
  }
  return(reMatrx)
}
