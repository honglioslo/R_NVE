## modify from spatialEco packages
## https://cran.r-project.org/web/packages/spatialEco/spatialEco.pdf
## select depends on methods
point.in.poly <- function (x, y, 
                           crs = CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"), 
                           poly.id = NULL, method = "in", n = 5) 
{

    library(rgeos)
    library(geosphere)
    if (!inherits(y, "SpatialPolygonsDataFrame")) 
        stop("y must be a SpatialPolygonsDataFrame object")
    if ((inherits(x, "SpatialPointsDataFrame") | inherits(x, 
        "SpatialPoints")) == FALSE) 
        stop("x must be a SpatialPointsDataFrame object")
    if (!is.null(poly.id)) {
        if (length(unique(y@data[, poly.id])) != nrow(y)) 
            stop("poly.id not unique")
    }
    if (is.null(poly.id)) {
        y@data$pids <- rownames(y@data)
        poly.id = "pids"
    }
			
	if (method == "nearest") {
	# only get the nearest point to centriod of the polygon
		centroids <- getSpPPolygonsLabptSlots(y)
		centroids <- data.frame(index = seq(dim(centroids)[1]), x = centroids[,1], y = centroids[,2])
		coordinates(centroids) <- ~x+y
		proj4string(centroids) <- crs_meter		
    centroids <-  spTransform(centroids, CRS("+init=epsg:4326"))		# WGS 84
    dis_points <- distGeo( spTransform(x, CRS("+init=epsg:4326")), centroids)
		z <- x[order(dis_points)[1:n],]	
		return(z)
	}	
	if (is.numeric(method) == TRUE) {
		y <- gBuffer(y, width = method)
	}	
	
  z <- x[!is.na(sp::over(x, sp::geometry(y))), ]
  return(z)
}