######################################################################################## 
#########################  CHAR2POL FUNCTION  ##########################################
########################################################################################

Char2Pol <- function(MultiPol_Char, plot=FALSE){
  MP_Test <- MultiPol_Char
  MP_Test <- str_remove_all(MP_Test, "[()MULTIPOLYGON]")
  MP_Test <- strsplit(MP_Test,",")
  MP_Test <- strsplit(MP_Test[[1]]," ")
  
  PolDataFrame <- data.frame(Lat = double(),
                             Lon = double(), 
                             stringsAsFactors=FALSE) 
  
  for(i in 1:length(MP_Test)){
    MP_Test[[i]]<- MP_Test[[i]][!(MP_Test[[i]][] %in% c(""))]
    PolDataFrame[i,1] <- MP_Test[[i]][1]
    PolDataFrame[i,2] <- MP_Test[[i]][2]
  }
  
  
  PolDataFrame <- data.matrix(PolDataFrame[], rownames.force = NA)
  PolDataFrame <- Polygon(PolDataFrame)
  PolDataFrame <- Polygons(c(PolDataFrame), "PolDataFrame")
  PolDataFrame <- SpatialPolygons(c(PolDataFrame), c(1:1), proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  if(plot == TRUE){
    mapview(PolDataFrame)
  }
  
  return(PolDataFrame)
  
}

######################################################################################## 
############################  PolOv FUNCTION  ##########################################
########################################################################################

PolOv <- function(S1_Pol, S2_Pol, Aoi){
  
  Int_S1S2 <- raster::intersect(S1_Pol, S2_Pol)
  Int_S1A <-  raster::intersect(S1_Pol, Aoi)
  Int_S2A <-  raster::intersect(S2_Pol, Aoi)
  Int_all <-  raster::intersect(Int_S1S2, Aoi)
  
  overlap_S1S2 <- (area(Int_S1S2)*100)/(area(S1_Pol))
  overlap_S1A <- (area(Int_S1A)*100)/(area(Aoi))
  overlap_S2A <- (area(Int_S2A)*100)/(area(Aoi))
  overlap_all <- (area(Int_all)*100)/(area(Aoi))
  
  Data <- c(overlap_S1S2,overlap_S1A,overlap_S2A,overlap_all)
  
  All_Pol <- do.call(bind, list(S1_Pol, S2_Pol, Aoi)) 
  
  mapview(All_Pol)
  
  return(Data)
  
}

######################################################################################## 
###############################  CLOUD COVER S2 FUNCTION  ##############################
########################################################################################






