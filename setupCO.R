
# rscript to read raw csv files and merge variables

library( fields)
setwd("~/Dropbox/Home/Projects/UrbanTypology")
source("GEOID2GISJOIN.R")

INCOME<- read.csv("data/Income.csv"      ,stringsAsFactors = FALSE )
ETHNICITY<- read.csv("data/Ethnicity.csv",stringsAsFactors = FALSE )
AGEGENDER<- read.csv("data/AgeGender.csv",stringsAsFactors = FALSE )
CNT<- read.csv("data/CNT_HT_CO for Doug.csv",
               stringsAsFactors = FALSE)
# strip out the quotes from CNT IDS
CNT$blkgrp <- substr(CNT$blkgrp,2,13 )
# sort out the labels


tract1<- substr( AGEGENDER$GISJOIN,10, 15)
blk1 <- substr( AGEGENDER$GISJOIN, 6,8)

blk2<- substr( CNT$blkgrp,4,6 )
tract2<- substr( CNT$blkgrp, 7, 12)

ID1<- paste0( blk1, tract1)
ID2<- paste0( blk2, tract2)
look21<- match( ID2, ID1)
look12<- match( ID1, ID2)

hold<- cbind( sort( CNT$blkgrp[is.na(look21)]),
              sort( AGEGENDER$GISJOIN[is.na(look12)]) )


# check
error <- as.numeric(sort(ID1[is.na(look12)]))-  as.numeric(sort(ID2[is.na(look21)])  )

sum( is.na( look21))
miss2<- is.na( look21)
# surgery to correct IDs
substr(blk2[miss2],3,3) <- "0"
ID1<- paste0( blk1, tract1)
ID2<- paste0( blk2, tract2)
index12<- match( ID1, ID2)

cat(" cases where substituting 0 in CNT ID",
    "does not give a match with the GISJOIN ids", fill=TRUE)
cat("This should be zero!", fill=TRUE)
print( sum( is.na( index12)))

# reorder CNT so that it matches the order of NHGIS

CNT <- CNT[index12,]


income<- INCOME$AF49E001
total<- ETHNICITY$AF2ME001
white<- ETHNICITY$AF2ME002
ageTotal<- AGEGENDER$AF2AE001
ageMale <- AGEGENDER$AF2AE002
ageMale25X49<- rowSums(AGEGENDER[c("AF2AE011",
                                  "AF2AE012",
                                  "AF2AE013",
                                  "AF2AE014",
                                  "AF2AE015")])
ageMale50X64<- rowSums(AGEGENDER[c("AF2AE016",
                                   "AF2AE017",
                                   "AF2AE018",
                                   "AF2AE019")])
ageMale65X<- rowSums(AGEGENDER[c("AF2AE020",
                                   "AF2AE021",
                                   "AF2AE022",
                                   "AF2AE023",
                                   "AF2AE024",
                                   "AF2AE025")]) 

ageFemale <- AGEGENDER$AF2AE026
ageFemale25X49<- rowSums(AGEGENDER[c("AF2AE035",
                                   "AF2AE036",
                                   "AF2AE037",
                                   "AF2AE038",
                                   "AF2AE039")])
ageFemale50X64<- rowSums(AGEGENDER[c("AF2AE040",
                                   "AF2AE041",
                                   "AF2AE042",
                                   "AF2AE043")])
ageFemale65X<- rowSums(AGEGENDER[c("AF2AE044",
                                   "AF2AE045",
                                   "AF2AE046",
                                   "AF2AE047",
                                   "AF2AE048",
                                   "AF2AE049")]) 

 
population <- CNT$population
size <- CNT$land_acres
populationDensity<- population/size
intersection<- CNT$intersection_density
age<- data.frame( ageMale25X49, ageMale50X64, ageMale65X,
                  ageFemale25X49, ageFemale50X64, ageFemale65X)
age <- age/ageTotal

CODemographic0 <- data.frame( INCOME$GISJOIN, population, populationDensity,
                              intersection,income, white,age  )

thisFile<- scan( "setupCO.R", what="a", sep='\r')
save(CODemographic0,age,population, thisFile, file="CO0.rda"  )

#### joined data test IDs Match 
 CO2<- read.csv("data/cbg_cnt_join.csv",
         stringsAsFactors = FALSE)
 CO2$GEOID<- as.character(CO2$GEOID )
 CO2$GISJOIN <- GEOID2GISJOIN(CO2$GEOID)
 look31<- match(CO2$GISJOIN, INCOME$GISJOIN )
 sum( is.na( look31))
 look13<- match( INCOME$GISJOIN,CO2$GISJOIN )
 sum( is.na( look13))
 
  testID<- ( cbind( sort(CO2$GISJOIN[is.na(look31)] ) [1:10],
 sort(INCOME$GISJOIN[is.na(look13)] ) [1:10])
      )
  
 testTRUE<- all(substr( testID[,1],2,14) == substr(testID[,2],3,15) )
  
 cat("comparision should test true:", testTRUE, fill=TRUE)
 
 
 