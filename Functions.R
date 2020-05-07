######################################################################################## 
#########################  CHAR2POL FUNCTION  ##########################################
########################################################################################
Char2Pol <- function(MultiPol_Char, Id = "no", plot=FALSE){
  
  MP_TEST <- MultiPol_Char
  MP_TEST <- str_remove_all(MP_TEST, "[()MULTIPOLYGON]")
  MP_TEST <- strsplit(MP_TEST,",")
  MP_TEST <- strsplit(MP_TEST[[1]]," ")
  
  MP_TESTDF <- data.frame(ID =  numeric(),
                          Lon = double(),
                          Lat = double(), 
                          stringsAsFactors=FALSE)
  
  for(i in 1:length(MP_TEST)){
    MP_TEST[[i]]<- MP_TEST[[i]][!(MP_TEST[[i]][] %in% c(""))]
    MP_TESTDF[i,2] <- MP_TEST[[i]][1]
    MP_TESTDF[i,3] <- MP_TEST[[i]][2]
  }
  
  MP_TESTDF$ID <- seq(from=1, to=length(MP_TEST))
  
  MP_TESTDF <- mutate_all(MP_TESTDF, function(x) as.numeric(as.character(x)))
  
  ch <- chull(MP_TESTDF$Lon,MP_TESTDF$Lat)
  
  xy <- as.matrix(MP_TESTDF[ch,c(2,3)])
  
  FinalPol <- coords2Polygons(xy, hole = NA, ID=c(Id))####wtf is c("1")
  
  crs(FinalPol) <- CRS("+proj=longlat +datum=WGS84")

  if(plot == TRUE){
    mapview(FinalPol)
  }
  
  return(FinalPol)
  
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
  
  return(Data)
  
}

######################################################################################## 
############################  View FUNCTION  ##########################################
########################################################################################

ViewMatch <- function(S1_Pol, S2_Pol, Aoi){
  #Reproject all Sp to match CRS
  
  All_Pol <- do.call(bind, list(S1_Pol, S2_Pol, Aoi)) 
  mapview(All_Pol)
  
}

######################################################################################## 
###############################  CLOUD COVER S2 FUNCTION  ##############################
########################################################################################






