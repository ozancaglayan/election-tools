
# Load external libraries -------------------------------------------------

library(sp)
library(rgdal)
library(ggplot2)
library(ggmap)

# Slightly modified version of spatialEco::hexagons function
# This samples a hexagonal grid from a given polygon map
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


# Load the map of Turkey in shapefile format ------------------------------

poli<-readOGR("data/shapefiles/", "Il", stringsAsFactors = F)
#plot(poli)


# Generate high number of hexagons ----------------------------------------

polihex <- hexagons(poli, n=20000)
#plot(polihex)
#plot(polihex, add=T) #overlay


# Assign each hex to a city and add to poly as metadata -------------------

city.hex.mapping <- over(polihex, poli)
city.hex.mapping$HEXID <- rownames(city.hex.mapping)
city.hex.mapping <- merge(polihex@data, city.hex.mapping, all.x=T)
polihex@data <- city.hex.mapping


# Generate a data.frame out of poly ---------------------------------------

gpoli <-fortify(polihex, region='HEXID')
gpoli$id <- as.integer(gpoli$id)

# Add province names
gpoli <- merge(gpoli,
               polihex@data[,c('HEXID', 'NAME')],
               all.x=T,
               by.x='id',
               by.y='HEXID')

gpoli.province <- fortify(polihex, region='NAME')

#a function to convert A:50% B:30% C:20%, nbins=10 into AAAAA, BBB, CC
repp <- function(vec, nbins) {
  as.character(cut(seq_len(nbins),
                   breaks = cumsum(c(0, as.numeric(vec))*(nbins/100)),
                   labels = names(vec)))
}


# Read vote shares for each province --------------------------------------

oy <- read.csv('data/2015/TR2015.csv', header = T, stringsAsFactors = F)
oy$others <- 100-apply(oy[,-c(1,6)], 1, sum) #make sure that rows sum up to 100
colnames(oy) <- c('province', 'AKP', 'CHP', 'MHP', 'HDP', 'Diğer')

#calculate number of hex bins per city
hex.counts <- by(gpoli,
                 gpoli$NAME,
                 function(df)length(unique(df$id)),
                 simplify = F)



# Generate repeating sequences of party names for each province -----------

il.hex.codes <- sapply(names(hex.counts),
                       function(il)repp(oy[oy$province==il,-1],
                                        hex.counts[[il]]))

# Get hexagon ids per province --------------------------------------------

il.hex.ids <- by(gpoli$id,
                 gpoli$NAME,
                 function(x)sort(unique(as.integer(x)), decreasing = F),
                 simplify = T)

# Merge repeating party sequences and hexagon ids -------------------------

il.parti.hex <- do.call(rbind, lapply(names(il.hex.ids),
                                      function(il)data.frame(il=rep(il, length(il.hex.ids[[il]])),
                                                             parti=il.hex.codes[[il]],
                                                             id=il.hex.ids[[il]],
                                                             stringsAsFactors = F)))

# Merge province-hex-party mapping into the main polygon data.frame -------

final.gpoli <- merge(gpoli, il.parti.hex, by='id', all.x=T)
province.centers <- as.data.frame(coordinates(poli)) #required to label provinces
names(province.centers) <- c('long', 'lat')
province.centers$il <- poli@data$NAME


# Plot the main figure ----------------------------------------------------

g <- ggplot(final.gpoli, aes(long, lat)) +
  geom_polygon(aes(group=id, fill=parti)) +
  geom_path(data=gpoli.province, aes(group=group), color="white")



# Generate fake text outlines ---------------------------------------------
# http://stackoverflow.com/a/10691826/1927108

theta <- seq(pi/8, 2*pi, length.out=16)
xo <- diff(range(province.centers$long))/1000
yo <- diff(range(province.centers$lat))/1000
for(i in theta) {
  g <- g + geom_text(data=province.centers,
    bquote(aes(x=long+.(cos(i)*xo),y=lat+.(sin(i)*yo),label=il)),
    size=4, colour='black' )
}


# Plot the rest of the figure ---------------------------------------------

g +
  #geom_text(data=province.centers, aes(label=il), color='#494949', size=4) +
  geom_text(data=province.centers, aes(label=il), color='white', size=4) +
  theme_nothing(legend=T) +
  scale_fill_manual(name='Parti', values=c('AKP'='#ff7f00',
                                           'CHP'='#e41a1c',
                                           'HDP'='#984ea3',
                                           'MHP'='#377eb8',
                                           'Diğer'='#4daf4a')) +
  coord_equal()

ggsave('oy-2015.png', width = 18, height = 8)

