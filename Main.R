############################################################################################################
###########            S1&S2 Decision Support Tool           ###############################################
########### Author                                           ###############################################
########### Nils Karges (s338495)                            ###############################################
########### nils.karges@stud-mail.uni-wuerzburg.de           ###############################################
############################################################################################################



############################################################################################################
############################   Set your path and define your area   ########################################
############################################################################################################

### Set wd

Fusion_Folder <- choose.dir() # set your wd
setwd(Fusion_Folder)

### Import Functions from Function.R File
source("Functions.R")

###create fusion_folder
dir.create(paste0(Fusion_Folder,"/Fusion_Output"), showWarnings = FALSE)
setwd(paste0(Fusion_Folder,"/Fusion_Output"))
Archive_Folder <- paste0(Fusion_Folder,"/Fusion_Output")

###install packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

### install libaries
packages <- c("sp","raster","rlist","getSpatialData","sf","sp","list","rSNAP","processx","dplyr","stringi",
              "installr","lubridate","rgdal","data.table","devtools","svDialogs","gdalUtils","Rcpp", "mapview",
              "mapedit","stringr","rgeos","rlang","officer","shiny","flextable","maps","mapproj","ggplot2","Orcs")

ipak(packages)

###call set_aoi() without argument, which opens a mapedit editor:
area <- mapview(editMap())
area <- area@object[[1]][[1]][[1]]

### polygonize the area and define crs
area <- Polygon(area)
area <- Polygons(c(area), "area")
area <- SpatialPolygons(c(area), c(1:1), proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))

### set login credentials and archive directory https://scihub.copernicus.eu/dhus/#/home
Username <- dlgInput("Enter your copernicus username:", default = "", Sys.info()["user"])$res

### set your username
if (!length(Username)) {# The user clicked the 'cancel' button
  cat("No Username entered")
} else {
  login_CopHub(username = Username)
}

### set timespan
TimeSpan_Start <- dlgInput("Enter start of timespan (YYYY-MM-DD) :", default = "", Sys.info()["user"])$res
TimeSpan_End <- dlgInput("Enter end of timespan (YYYY-MM-DD) :", default = "", Sys.info()["user"])$res

###set archive folder
set_archive(Archive_Folder)

### Use getSentinel_query to search for data (using the session AOI)
Sentinel_2 <- getSentinel_query(time_range = c(TimeSpan_Start , TimeSpan_End), platform = "Sentinel-2", aoi = area) #or "Sentinel-1" or "Sentinel-3" or "Sentinel-5p"
Sentinel_1 <- getSentinel_query(time_range = c(TimeSpan_Start , TimeSpan_End), platform = "Sentinel-1", aoi = area) #or "Sentinel-1" or "Sentinel-3"

### Filter the records
Sentinel_2_filtered <- Sentinel_2[which(Sentinel_2$processinglevel == "Level-2A"),] #filter by Level
Sentinel_2_filtered <- Sentinel_2_filtered[as.numeric(Sentinel_2_filtered$cloudcoverpercentage) <= 50, ] #filter by clouds
Sentinel_1_filtered <- Sentinel_1[which(Sentinel_1$producttype == "GRD"),] #format

##########################################################################################################
############################   Find the Fusion pairs   ###################################################
##########################################################################################################

### Store the row.name to use it with getSentinel_data
Sentinel_1_filtered$ID <- row.names(Sentinel_1_filtered)
Sentinel_2_filtered$ID <- row.names(Sentinel_2_filtered)

### Filter data to match DATES in new DF
Sentinel_1_filtered <- subset(Sentinel_1_filtered, select= c("ID","title","url","url.icon","ingestiondate","polarisationmode","orbitdirection","footprint"))
Sentinel_2_filtered <- subset(Sentinel_2_filtered, select = c("ID", "title","url","url.icon","ingestiondate","orbitnumber", "cloudcoverpercentage"))

### Modify the date from "2018-09-29T16:18:51.033Z" -> "2018-09-29"
Sentinel_1_filtered$ingestiondate <- substr(Sentinel_1_filtered$ingestiondate,1,10)
Sentinel_2_filtered$ingestiondate <- substr(Sentinel_2_filtered$ingestiondate,1,10)

### Reset Row.names
rownames(Sentinel_1_filtered) <- NULL
rownames(Sentinel_2_filtered) <- NULL

### Unique S1 date Images based on high ovelap with the AOI
Dupl_S1Dates <- Sentinel_1_filtered[duplicated(Sentinel_1_filtered$ingestiondate),5]

### Make a list of unique S2 Dates 
List_Date_S2 <- unique(Sentinel_2_filtered$ingestiondate)

### For loop to pick the lowest cloud cover of same date images S2 ( again clouds removal to remove double dates)
for(i in 1:length(List_Date_S2)){
  
  ### Make a list to get the row.name where ingestiondate == unique dates 
  Tem_List <- which(Sentinel_2_filtered$ingestiondate == List_Date_S2[i], arr.ind=T)
  
  
  ### Get the row.name min val in each list
  Temporal_Min <- min(Sentinel_2_filtered$cloudcoverpercentage[Tem_List])
  
  ### produces temporal index
  Temporal_Index <- which(Temporal_Min == Sentinel_2_filtered$cloudcoverpercentage[Tem_List], arr.ind=T)
  
  ### index
  
  Index <- Temporal_Index
  
  ### producing the final list
  if(i == 1){
    Temp_DataF <- Sentinel_2_filtered[Tem_List[Index],]
    Sentinel_2_filtered_F <- Temp_DataF
    
  } else{
    Temp_DataF <- Sentinel_2_filtered[Tem_List[Index],]
    Sentinel_2_filtered_F <- rbind(Sentinel_2_filtered_F,Temp_DataF)
    
  }
  
}

Sentinel_2_filtered <- Sentinel_2_filtered_F
rm(Sentinel_2_filtered_F)


###########################################################################################################
#####################Make comparison by Day timespan between S2 and S1 ####################################
###########################################################################################################

###Finding the closest Date
Sentinel_1_filtered$ClosestS2Date <- 0

### Connect the ID?s
Final_S1_ID <- Sentinel_1_filtered$ID

### Temporal ID for S2
Final_S2_ID <- Sentinel_1_filtered$ID

for(i in 1:length(Sentinel_1_filtered$ingestiondate)){
  date <- as.Date(Sentinel_1_filtered$ingestiondate[i])
  Final_S2_ID[i] <-  Sentinel_2_filtered$ID[which.min(abs(date - as.Date(Sentinel_2_filtered$ingestiondate)))]
}

Sentinel_1 <- Sentinel_1[Final_S1_ID,]
Sentinel_2 <- Sentinel_2[unique(Final_S2_ID),]

##########################################################################################################
###########################Match Dataframe################################################################
##########################################################################################################

### Build match dataframe to construct the GUI
Match_df <- as.data.frame(Final_S1_ID, stringsAsFactors = FALSE)
Match_df$S1_Date <- as.Date(Sentinel_1_filtered$ingestiondate)
Match_df$S2_ID <- as.integer(Final_S2_ID)

Match_df$S2_Date <- as.Date(substr(Sentinel_2[Final_S2_ID,grep("ingestiondate", colnames(Sentinel_2))],1,10))

### setting row names
names(Match_df) <- c("S1_ID","S1_Date","S2_ID","S2_Date")

### set match df integer
Match_df$S1_ID <- as.integer(Final_S1_ID)

Match_df$DateDiff <- abs(difftime(Match_df$S1_Date, Match_df$S2_Date , units = c("days")))

### Set columnames 0
Match_df$S1S2OV <- "--"
Match_df$S1S2D <- 0
Match_df$S1AD <- 0
Match_df$S2AD <- 0
Match_df$AllD <- 0

### Inception
for(i in 1: length(Match_df$S1_ID)){
  
  TempData <- PolOv(Char2Pol(Sentinel_1$footprint[Match_df$S1_ID[i] == row.names(Sentinel_1)],),
                    Char2Pol(Sentinel_2$footprint[Match_df$S2_ID[i] == row.names(Sentinel_2)],),
                    area)
  
  
  Match_df$S1S2D[i]  <- TempData[1]
  Match_df$S1AD[i]   <- TempData[2]
  Match_df$S2AD[i]   <- TempData[3]
  Match_df$AllD[i]   <- TempData[4]
  Match_df$S1S2OV[i] <- TempData[5]
  
}

###Delete nonS1S2 overay matches
Match_df <- subset(Match_df, S1S2OV!="FALSE")

### View poligon overay
ViewMatch(1,Match_df,Sentinel_1,Sentinel_2,area) 


### now open the Match_df and watch your statistics


################################################################################################
################################### Download the Data ##########################################
################################################################################################

## Download some datasets to your archive directory
#datasets <- getSentinel_data(records =  Sentinel_2[unique(Final_S2_ID), ])
#datasets <- getSentinel_data(records =  Sentinel_1[unique(Final_S1_ID), ])

DownloadList <- c(1)

datasets <- getSentinel_data(records =  Sentinel_1[row.names(Sentinel_1) == Match_df$S1_ID[DownloadList],] , dir_out = Archive_Folder)
datasets <- getSentinel_data(records =  Sentinel_2[row.names(Sentinel_2) == Match_df$S2_ID[DownloadList],] , dir_out = Archive_Folder)

datasets_prep <- prepSentinel(datasets, format = "tiff")

################################################################################################
##############################################Finite############################################
################################################################################################
