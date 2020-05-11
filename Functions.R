#################################################################################################### 
#########################  CHAR2POL FUNCTION  ###################################################### 
#################################################################################################### 
Char2Pol <- function(MultiPol_Char, Id = "no", plot=FALSE){
  
  # Create a list of coordinates from footprint character 
  MP_TEST <- MultiPol_Char
  MP_TEST <- str_remove_all(MP_TEST, "[()MULTIPOLYGON]")
  MP_TEST <- strsplit(MP_TEST,",")
  MP_TEST <- strsplit(MP_TEST[[1]]," ")
  
  # Create an empty dataframe to format the coordinates 
  MP_TESTDF <- data.frame(ID =  numeric(),
                          Lon = double(),
                          Lat = double(), 
                          stringsAsFactors=FALSE)
  
  # Due to not all multi polygon are equally formatted and additional character cleaning
  # is needed, the for loop goes through all elements in list deleting c("") elements
  # to isolate the coordinates
  for(i in 1:length(MP_TEST)){
    MP_TEST[[i]]<- MP_TEST[[i]][!(MP_TEST[[i]][] %in% c(""))]
    MP_TESTDF[i,2] <- MP_TEST[[i]][1]
    MP_TESTDF[i,3] <- MP_TEST[[i]][2]
  }
  
  # Add ID field matching the length of the coordinate list
  MP_TESTDF$ID <- seq(from=1, to=length(MP_TEST))
  
  # Convert aquired values to numerical
  MP_TESTDF <- mutate_all(MP_TESTDF, function(x) as.numeric(as.character(x)))
  
  # Compute convex hull
  ch <- chull(MP_TESTDF$Lon,MP_TESTDF$Lat)
  
  # Create coordinate matrix from dataframe matching convex hull  
  xy <- as.matrix(MP_TESTDF[ch,c(2,3)])
  
  # Due to some polygons will have 3 points all warnings should be not shown 
  options(warn=-1)
  
  # Convert coordinates from matrix to polygon
  FinalPol <- coords2Polygons(xy, hole = NA, ID=c(Id))
  
  # start warnings again
  options(warn=0)
  
  # assign coordinate system
  crs(FinalPol) <- CRS("+proj=longlat +datum=WGS84")

  # Plot polygon if called with TRUE 
  if(plot == TRUE){
    mapview(FinalPol)
  }
  
  # Return created polygon
  return(FinalPol)
  
}

####################################################################################################  
############################  PolOv FUNCTION  ###################################################### 
#################################################################################################### 

PolOv <- function(S1_Pol, S2_Pol, Aoi){
  
  # Assign same coordinate system to all polygons 
  crs(S1_Pol) <- CRS("+proj=longlat +datum=WGS84")
  crs(S2_Pol) <- CRS("+proj=longlat +datum=WGS84")
  crs(Aoi)    <- CRS("+proj=longlat +datum=WGS84")
  
  # Calculate % area of intersection between S1 and Aoi scenes 
  Int_S1A <-  raster::intersect(S1_Pol, Aoi)
  overlap_S1A <- (area(Int_S1A)*100)/(area(Aoi))
  
  # Calculate % area of intersection between S2 and Aoi scenes
  Int_S2A <-  raster::intersect(S2_Pol, Aoi)
  overlap_S2A <- (area(Int_S2A)*100)/(area(Aoi))
  

  # Due to some polygons will not overlap warnings should be not shown
  options(warn=-1)
  
  # Calculate % area of intersection between S1 and S2 scenes
  Int_S1S2 <-  raster::intersect(S1_Pol, S2_Pol)
  
  # Check if intersection value is null, if so assign overlap areas to 0
  # and change "Status" to FALSE. If areas overlap, calculate the overlaping
  # % and set "Status" to TRUE  
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
  
  # start warnings again
  options(warn=0)

  # Create 2 decimal numerical vector of % overlaping areas  
  Data <- c(round(overlap_S1S2,2),round(overlap_S1A,2),round(overlap_S2A,2),round(overlap_all,2),Status) 
  
  #Return values 
  return(Data)
  
}

####################################################################################################  
############################  View FUNCTION  ###################################################### 
################################################################################################### 
ViewMatch <- function(id, MchDf, S1Df, S2Df, Aoi){
  
  # Apply Char2Pol to the footprint of Sentinel 1 & 2 that matches the 
  # id given in the Match dataframe
  S1POl <- Char2Pol(S1Df$footprint[MchDf$S1_ID[id] == rownames(S1Df) ],"S1")
  S2POl <- Char2Pol(S2Df$footprint[MchDf$S2_ID[id] == rownames(S2Df) ],"S2")
  
  #Reproject all Sp to match CRS
  crs(S1POl) <- CRS("+proj=longlat +datum=WGS84")
  crs(S2POl) <- CRS("+proj=longlat +datum=WGS84")
  crs(Aoi)   <- CRS("+proj=longlat +datum=WGS84")
  
  # Merge all polygons in single list
  All_Pol <- do.call(bind, list(S1POl, S2POl, Aoi)) 
  
  # Plot all 3 polygons in the interactive mapview layout
  mapview(All_Pol)
  
}

####################################################################################################  
###############################  CLOUD COVER S2 FUNCTION  ########################################## 
#################################################################################################### 




