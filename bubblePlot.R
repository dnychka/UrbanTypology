bubblePlot= function( x,y=NULL, z,col=tim.colors(256), ...){
  ctab= color.scale( z, col)
  points( x,y, col=ctab,pch=16,...)
  image.plot( legend.only=TRUE,add=TRUE, 
              col=col, zlim =range( z, na.rm=TRUE))
}