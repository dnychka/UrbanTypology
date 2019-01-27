GEOID2GISJOIN <- function( ID){
  ID<- as.character(ID)
  state<- substr(ID, 1,2)
  county<- substr( ID, 3,5)
  tract<- substr( ID, 6,11)
  blkgrp<- substr(ID, 12,14)
 
   ID2<- paste0("G",state,"0",county,"0",
               tract, blkgrp)
  return( ID2)
  
}