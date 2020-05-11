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
  
  options(warn=-1)
  
  FinalPol <- coords2Polygons(xy, hole = NA, ID=c(Id))
  
  options(warn=0)
  
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
  #
  crs(S1_Pol) <- CRS("+proj=longlat +datum=WGS84")
  crs(S2_Pol) <- CRS("+proj=longlat +datum=WGS84")
  crs(Aoi)    <- CRS("+proj=longlat +datum=WGS84")
  
  #
  Int_S1A <-  raster::intersect(S1_Pol, Aoi)
  overlap_S1A <- (area(Int_S1A)*100)/(area(Aoi))
  
  #
  Int_S2A <-  raster::intersect(S2_Pol, Aoi)
  overlap_S2A <- (area(Int_S2A)*100)/(area(Aoi))
  
  #############
  
  options(warn=-1)
  
  #
  Int_S1S2 <-  raster::intersect(S1_Pol, S2_Pol)
  if(is.null(Int_S1S2)){
    overlap_S1S2 <- 0
    overlap_all <- 0
    Status <- "FALSE"
  } else{
    overlap_S1S2 <- (area(Int_S1S2)*100)/(area(S1_Pol))
    Int_All <-  raster::intersect(Int_S1S2, Aoi)
    overlap_all <- (area(Int_All)*100)/(area(Aoi))
    Status <- "TRUE"
  }
  
  options(warn=0)

  Data <- c(round(overlap_S1S2,2),round(overlap_S1A,2),round(overlap_S2A,2),round(overlap_all,2),Status) 
  
  return(Data)
  
}

######################################################################################## 
############################  View FUNCTION  ##########################################
########################################################################################

ViewMatch <- function(id, MchDf, S1Df, S2Df, Aoi){
  
  S1POl <- Char2Pol(S1Df$footprint[MchDf$S1_ID[id] == rownames(S1Df) ],"S1")
  S2POl <- Char2Pol(S2Df$footprint[MchDf$S2_ID[id] == rownames(S2Df) ],"S2")
  
  #Reproject all Sp to match CRS
  crs(S1POl) <- CRS("+proj=longlat +datum=WGS84")
  crs(S2POl) <- CRS("+proj=longlat +datum=WGS84")
  crs(Aoi)   <- CRS("+proj=longlat +datum=WGS84")
  
  All_Pol <- do.call(bind, list(S1POl, S2POl, Aoi)) 
  
  mapview(All_Pol)
  
}

######################################################################################## 
###############################  CLOUD COVER S2 FUNCTION  ##############################
########################################################################################






