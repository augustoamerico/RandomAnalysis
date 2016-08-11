library("rgdal")
library("geosphere")
library(rworldmap)
library(ggmap)

dataPortinerario <- rgdal::readOGR(
  dsn=path.expand("E:\\RandomAnalysis\\portinerario\\data\\Portinerário.kml"), 
  layer="cst",
  encoding="UTF-8"
) 

df <- data.frame(
  lon = dataPortinerario@coords[,"coords.x1"],
  lat = dataPortinerario@coords[,"coords.x2"],
  name = dataPortinerario@data$Name
)

df$color <-  NA
df$color <- c(rep('red', length(df$lon)/2),rep('blue', length(df$lon)/2))


geo.dist = function(df) {
  require(geosphere)
  d <- function(i,z){         # z[1:2] contain long, lat
    dist <- rep(0,nrow(z))
    dist[i:nrow(z)] <- distHaversine(z[i:nrow(z),1:2],z[i,1:2])
    return(dist)
  }
  dm <- do.call(cbind,lapply(1:nrow(df),d,df))
  return(as.dist(dm))
}

d      <- geo.dist(df)   # distance matrix
km <- kmeans(df[, !(names(df) %in% c('name','color'))], centers=4)
km <- kmeans(d, centers=4)
length(km$cluster)

clusterResultWithData <- data.frame(
  lon = df$lon,
  lat = df$lat,
  color = km$cluster
)
 

map <- get_map(location = 'Oporto', zoom = 14)
ggmap(map)
mappoints <- ggmap(map) + geom_point(data = df, aes(x = lon, y = lat))


qmplot(lon, lat, data = clusterResultWithData, colour = clusterResultWithData$color, size = I(3), darken = .3, legend = "top")
