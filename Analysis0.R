library( fields)
library(rgl)

setwd("~/Home/Projects/UrbanTypology")
load("CO0.rda")
load("COMapInfo.rda")

look  <- na.omit(CODemographic0[,-1] ) 
look2 <- scale(as.matrix( look))
dimnames(look2) <- list(NULL, names( look) )

ageSVD<- svd( look2[,6:11])

svdCO <- svd( look2)
plot( sqrt( cumsum(svdCO$d^2)/ sum( svdCO$d^2) ) )

ctabPop<- color.scale((look2[,"population"]), col=rainbow(256) )
Z<- cbind(
  (look[, "populationDensity"]),
  (look[,"income"]), 
  ageSVD$u[,1]
  )


plot3d( Z, axes=TRUE, box=FALSE, xlab="Population Density",
        ylab="Income", zlab="Age Structure",
        col=ctabPop)


U <- svdCO$u%*%diag(svdCO$d)
V<- svdCO$v
dimnames( V)<- list( names( look),NULL)
 
kMeans<- kmeans( U, 6 )
ctab<- two.colors(6)[kMeans$cluster]
# CSM 39.7510° N, 105.2226° W
distCSM<- rdist.earth( cbind(-105.2226,39.7510),COCentroids  )
indCSM<- which.min( c(distCSM ) )
ctab[indCSM]<-"magenta"
#plot3d( svdCO$u[,1:3], col=ctab, size=4)
Z<- cbind(
  (look[, "populationDensity"]),
  (look[,"income"])/1000, 
  ageSVD$u[,1]
)
plot3d( Z,  box=TRUE, 
        col=ctab, size = .5,axes=FALSE,
        type = "s" )
#labels=c("Population Density",
#         "Income","Age Structure")
axes3d( edges=c("x-+", "y+-", "z+-") )



par( mar=c( 5,5,3,6))
coords<- rbind( c( -105.4694,39.88133),
                c(-105.0299, 40.21773)
                )
plot( coords, type="n")
rasterImage(BoulderMap$myTile,-105.4694,39.88133,-105.0299, 40.21773)
points( COCentroids[,1], COCentroids[,2],
        col=ctab, pch=16)


fit <- cmdscale(dist( U ), eig=TRUE, k=2)
plot( fit$points, col= ctab)

fit <- cmdscale(dist( U ), eig=TRUE, k=2)
plot3d( fit$points, col= ctab)



