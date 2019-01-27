
N<- length( COPolygons)
plot( c(-105.4,-105.1),
      c(39.9, 40.2), type="n")
for ( k in 1:N){
  polygon(COPolygons[[k]] )
}
points( COCentroids, col="red", pch=16)

bubblePlot( COCentroids, z=CODemographic0$income)


par( mar=c( 5,5,3,6))
plot( lon, lat, type="n")
rasterImage(boulderMap$myTile,-105.4694,39.88133,-105.0299, 40.21773)
points( COCentroids[,1], COCentroids[,2])

bubblePlot( COCentroids, z=CODemographic0$income/1000, cex=2.0)