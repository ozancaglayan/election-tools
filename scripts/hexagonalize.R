library(sp)
library(rgdal)
library(ggplot2)

#slightly modified version of spatialEco::hexagons function
hexagons <- function (x, n = 1000, ...)
{
  if (!inherits(x, "SpatialPointsDataFrame") &
      !inherits(x, "SpatialPolygonsDataFrame"))

    stop("x MUST BE AN sp SpatialDataFrame OBJECT")
  pts <- sp::spsample(x, type = "hexagonal", n = n, ...)
  genPolyList <- function(hexGrid, dx) {
    if (missing(dx))
      dx <- 2 * min(diff(sort(unique(hexGrid$x))))
    dy <- dx/sqrt(3)
    x.offset <- c(-dx/2, 0, dx/2, dx/2, 0, -dx/2, -dx/2)
    y.offset <- c(dy/2, dy, dy/2, -dy/2, -dy, -dy/2, dy/2)
    f <- function(i) list(x = hexGrid$x[i] + x.offset, y = hexGrid$y[i] +
                            y.offset)
    ret <- lapply(1:length(hexGrid$x), f)
  }
  ret <- genPolyList(data.frame(sp::coordinates(pts)))
  npoly <- length(ret)
  Srl <- vector(mode = "list", length = npoly)
  IDS <- 1:npoly
  for (i in 1:npoly) Srl[[i]] <- sp::Polygons(list(sp::Polygon(ret[[i]])),
                                              IDS[i])
  res <- sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(Srl),
                                      data = data.frame(HEXID = IDS))
  sp::proj4string(res) <- sp::CRS(sp::proj4string(x))
  return(res)
}

poli<-readOGR("data/shapefiles/", "Il", stringsAsFactors = F)
plot(poli)

polihex <- hexagons(poli, n=10000)
plot(polihex)
#plot(polihex, add=T) #overlay

city.hex.mapping <- over(polihex, poli)
city.hex.mapping$HEXID <- rownames(city.hex.mapping)
city.hex.mapping <- merge(polihex@data, city.hex.mapping, all.x=T)
polihex@data <- city.hex.mapping

gpoli <-fortify(polihex, region='HEXID')
#add province names
gpoli <- merge(gpoli, polihex@data[,c('HEXID', 'NAME')], all.x=T, by.x='id', by.y='HEXID')

ggplot(gpoli) +
  aes(long,lat,group=id) +
  geom_polygon() +
  geom_path(color="white") +
  coord_equal()


oy <- read.csv('data/2015/TR2015.csv', header = T, stringsAsFactors = F)
