library(rgdal)
library(sf)
library( fields)
library(RgoogleMaps)
setwd("~/Dropbox/Home/Projects/UrbanTypology")
source("GEOID2GISJOIN.R")
APIKey <- 'Has been omitted for privacy'
# See APIKey.R local source code.
# reading in shape files does not need the key.

setwd("~/Dropbox/Home/Projects/UrbanTypology")

COblkgrp<- readOGR("cb_2016_08_bg_500k/cb_2016_08_bg_500k.shp")

look<- COblkgrp@polygons
N<-length(look)
COCentroids<- matrix(NA,N,2)
COPolygons<- NULL
for( k in 1:N){
  temp <- (look[[k]]@Polygons)
  COCentroids[k,] <- colMeans(temp[[1]]@coords)
  COPolygons<- c(COPolygons, list( temp[[1]]@coords ) )
}
GISJOIN<- GEOID2GISJOIN(COblkgrp$GEOID)
COblkgrp$GISJOIN <- GISJOIN

load("CO0.rda")
ind12 <- match( as.character(CODemographic0$INCOME.GISJOIN),
                      COblkgrp$GISJOIN) 
############### sum should be zero for match.
sum( is.na( ind12))

COblkgrp$GISJOIN[ind12[1]] 
as.character( CODemographic0$INCOME.GISJOIN[1])

dimnames( COCentroids)<- list(as.character(COblkgrp$GISJOIN) ,NULL)
COCentroids<- COCentroids[ind12,]
COPolygons<- COPolygons[ind12]

latCO<- range(COCentroids[,2] )
lonCO <- range( COCentroids[,1])
zoomCO <- min(MaxZoom(range(latCO), range(lonCO)));

COMap<- GetMap(center= colMeans( COCentroids)[2:1] , zoom=zoomCO,  
                    API_console_key= APIKey)
#plot( lonCO, latCO, type="n")
#rasterImage(COMap$myTile,
#            lonCO[1],latCO[1],lonCO[2],latCO[2])



latB =  c(39.9, 40.2)
lonB = c(-105.4,-105.1)
centerB = c(mean(latB), mean(lonB));
zoomB <- min(MaxZoom(range(latB), range(lonB)));
BoulderMap<- GetMap(center=centerB, zoom=zoomB,  
                    API_console_key= APIKey)
pdf("testBoulderMap.pdf", width = 6, height=6)
plot( lonB, latB, type="n")
rasterImage(BoulderMap$myTile,
            lonB[1],latB[1],lonB[2],latB[2])
for( k in 1: length( COPolygons)){
  temp<- COPolygons[[k]]
  polygon(temp[,1], temp[,2], lwd=.5,col= NA,
          border="magenta")
}
dev.off()


save( COMap,BoulderMap, 
      lonB, latB, 
      lonCO, latCO,
      zoomCO, 
      COCentroids, COPolygons,
      file="COMapInfo.rda")







