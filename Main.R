############################################################################################################
############################################################################################################
##################################### S1&S2_Fusion #########################################################
############################################################################################################
############################################################################################################
#setting wd
Fusion_Folder <-  "D:/FUSION2020"  #setwd(choose.dir())
dir.create(paste0(Fusion_Folder,"/Fusion_Output"), showWarnings = FALSE)
setwd(paste0(Fusion_Folder,"/Fusion_Output"))
#Archive_Folder <- paste0(Fusion_Folder,"/Fusion_Output")

###install packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c("sp","raster","rlist","getSpatialData","sf","sp","list","dplyr","lubridate","rgdal","data.table","devtools","svDialogs","gdalUtils","Rcpp")
ipak(packages)

###call set_aoi() without argument, which opens a mapedit editor:
set_aoi() 

### set login credentials and archive directory https://scihub.copernicus.eu/dhus/#/home
Username <- dlgInput("Enter your copernicus username:", default = "", Sys.info()["user"])$res

if (!length(Username)) {# The user clicked the 'cancel' button
  cat("No Username entered")
} else {
  login_CopHub(username = Username)
}


TimeSpan_Start <- dlgInput("Enter start of timespan (YYYY-MM-DD) :", default = "", Sys.info()["user"])$res
TimeSpan_End <- dlgInput("Enter end of timespan (YYYY-MM-DD) :", default = "", Sys.info()["user"])$res

set_archive(Archive_Folder)

### Use getSentinel_query to search for data (using the session AOI)
Sentinel_2 <- getSentinel_query(time_range = c(TimeSpan_Start , TimeSpan_End), platform = "Sentinel-2") #or "Sentinel-1" or "Sentinel-3"
Sentinel_1 <- getSentinel_query(time_range = c(TimeSpan_Start , TimeSpan_End), platform = "Sentinel-1") #or "Sentinel-1" or "Sentinel-3"

### Filter the records
###see all available filter attributes
unique(Sentinel_1$processinglevel) #use one of the, e.g. to see available processing levels
unique(Sentinel_2$processinglevel)

Sentinel_2_filtered <- Sentinel_2[which(Sentinel_2$processinglevel == "Level-1C"),] #filter by Level
Sentinel_2_filtered <- Sentinel_2_filtered[as.numeric(Sentinel_2_filtered$cloudcoverpercentage) <= 20, ] #filter by clouds
Sentinel_1_filtered <- Sentinel_1[which(Sentinel_1$producttype == "GRD"),]

########################################################################################
############################   Find the Fusion pairs   #################################
########################################################################################

### Duplicate dataframes 
Sentinel_1_filteredD <- Sentinel_1_filtered
Sentinel_2_filteredD <- Sentinel_2_filtered


### Store the row.name to use it with getSentinel_data
Sentinel_1_filteredD$ID <- row.names(Sentinel_1_filteredD)
Sentinel_2_filteredD$ID <- row.names(Sentinel_2_filteredD)


### Filter data to match DATES in new DF
Sentinel_1_filteredD <- subset(Sentinel_1_filteredD, select= c("ID","title","url","url.icon","ingestiondate","polarisationmode"))
Sentinel_2_filteredD <- subset(Sentinel_2_filteredD, select = c("ID", "title","url","url.icon","ingestiondate","orbitnumber", "cloudcoverpercentage"))

### Modify the date from "2018-09-29T16:18:51.033Z" -> "2018-09-29"
Sentinel_1_filteredD$ingestiondate <- substr(Sentinel_1_filteredD$ingestiondate,1,10)
Sentinel_2_filteredD$ingestiondate <- substr(Sentinel_2_filteredD$ingestiondate,1,10)

### Reset Row.names
rownames(Sentinel_1_filteredD) <- NULL
rownames(Sentinel_2_filteredD) <- NULL

### Unique S1 date Images
Sentinel_1_filteredD_F <- Sentinel_1_filteredD[!duplicated(Sentinel_1_filteredD$ingestiondate),]

### Make a list of unique S2 Dates 
List_Date_S2 <- unique(Sentinel_2_filteredD$ingestiondate)


### For loop to pick the lowest cloud cover of same date images S2 ( again clouds removal to remove double dates)
for(i in 1:length(List_Date_S2)){
  
  ### Make a list to get the row.name where ingestiondate == unique dates 
  Tem_List <- which(Sentinel_2_filteredD$ingestiondate == List_Date_S2[i], arr.ind=T)
  #print(Tem_List)
  
  ### Get the row.name min val in each list
  Temporal_Min <- min(Sentinel_2_filteredD$cloudcoverpercentage[Tem_List])
  
  ### produces temporal index
  Temporal_Index <- which(min(Sentinel_2_filteredD$cloudcoverpercentage[Tem_List]) == Sentinel_2_filteredD$cloudcoverpercentage[Tem_List], arr.ind=T)
  
  Index <- Temporal_Index[[1]][1]
  
  ### producing the final list
  if(i == 1){
    Temp_DataF <- Sentinel_2_filteredD[Tem_List[Index],]
    Sentinel_2_filteredD_F <- Temp_DataF
    
  } else{
    Temp_DataF <- Sentinel_2_filteredD[Tem_List[Index],]
    Sentinel_2_filteredD_F <- rbind(Sentinel_2_filteredD_F,Temp_DataF)
    
  }
  
}

###########################################################################################
### Make comparison Day timespan between S2 and S1 (S2 FIRST BECAUSE IS LESS DATA) #
###########################################################################################

###Finding the closest Date
Sentinel_1_filteredD_F$ClosestS2Date <- 0

# Connect the ID?s
Final_S1_ID <- Sentinel_1_filteredD_F$ID

# Temporal ID for S2
Final_S2_ID <- Sentinel_1_filteredD_F$ID

###
for(i in 1:length(Sentinel_1_filteredD_F$ingestiondate)){
  
  date <- as.Date(Sentinel_1_filteredD_F$ingestiondate[i])
  
  Final_S2_ID[i] <-  Sentinel_2_filteredD_F$ID[which.min(abs(date - as.Date(Sentinel_2_filteredD_F$ingestiondate)))]
  
  
}

Sentinel_1 <- Sentinel_1[Final_S1_ID,]

Sentinel_2 <- Sentinel_2[unique(Final_S2_ID),]

######################################################################################## 
###########################Match Dataframe##############################################
########################################################################################

Match_df <- as.data.frame(Sentinel_1_filteredD_F$ingestiondate)
Match_df$Sent_2_Date <- Sentinel_2_filteredD_F$ingestiondate
names(Match_df) <- c("S1_Date","S2_Date")

########################################################################################
################################### Download the Data ##################################
########################################################################################

## Download some datasets to your archive directory
#datasets <- getSentinel_data(records =  Sentinel_2[unique(Final_S2_ID), ])
#datasets <- getSentinel_data(records =  Sentinel_1[unique(Final_S1_ID), ])

if(file.exists(paste0(Sentinel_2[Final_S2_ID[1], ]$title,".zip")) == FALSE){
  datasetsS2 <- getSentinel_data(records =  Sentinel_2[Final_S2_ID[1], ], dir_out = getwd())
}

if(file.exists(paste0(Sentinel_1[Final_S1_ID[1], ]$title,".zip")) == FALSE){
  datasetsS1 <- getSentinel_data(records =  Sentinel_1[Final_S1_ID[1], ], dir_out = getwd())
}

#datasets <- getSentinel_data(records = records_filtered[c(4,7,9), ])

## Finally, define an output format and make them ready-to-use
datasets_prep_s2 <- prepSentinel(datasetsS2, format = "tiff", dir_out = getwd())
# or use VRT to not store duplicates of different formats
datasets_prep_s1 <- prepSentinel(datasetsS1, format = "vrt", dir_out = getwd())

## View the files
datasets_prep[[1]][[1]][1] #first dataset, first tile, 10 m resolution
datasets_prep[[1]][[1]][2] #first dataset, first tile, 20 m resolution
datasets_prep[[1]][[1]][3] #first dataset, first tile, 60 m resolution

## Load them directly into R
r <- stack(datasets_prep[[1]][[1]][1])





##############################################################################################
######################################Preprocessing S1########################################
##############################################################################################
#(Kemeng </3)
devtools::install_github("https://github.com/Shirobakaidou/rSNAP")
install.packages("stringi")
library(stringi)
library(rSNAP)
library(processx)
preprocessing(sourcePath = "D:/RS1_2_Fusion/Fusion_Output/get_data/Sentinel-1",
              wd = "D:/RS1_2_Fusion/Fusion_Output/Sentinel_1_Prep",
              pixelSpacingInMeter=10,
              windowSize= "5x5",
              geoRegion = "9.776839256286621 49.75139617919922, 10.520546913146973 49.75139617919922, 10.520546913146973 48.97669982910156, 9.776839256286621 48.97669982910156, 9.776839256286621 49.75139617919922")

### Polygon example
#POLYGON ((9.776839256286621 49.75139617919922, 10.520546913146973 49.75139617919922, 10.520546913146973 48.97669982910156, 9.776839256286621 48.97669982910156, 9.776839256286621 49.75139617919922, 9.776839256286621 49.75139617919922))</


s1 <- raster("D:/RS1_2_Fusion/Fusion_Output/Sentinel_1_Prep/Output/1_S1B_IW_GRDH_1SDV_20190619.tif")

crs_s1 <- crs(s1)

###extent of s1
extent(s1)

ext_s1 <- extent(s1)
plot(s1)

s1

##############################################################################################
######################################Preprocessing S2########################################
##############################################################################################
###Load S2 Bands
### choose manually wd from s2  img
#setwd(choose.dir())

#Unzip S2 images


setwd("D:/FUSION2020/Fusion_Output/S2")
###check crs

S2_Bands <- list.files(getwd())

s2a <- readGDAL(S2_Bands[1])



################################################################ 
################################################################ FROM JP2 2 tiff

### set path
jp2_path <-"D:/RS1_2_Fusion/jp2_2/" #setwd(choose.dir())

###load jp2 stack
jp2_to_raster <- function(jp2_path, ref_crs) {
  # read ".jp2" and convert to raster
  sen2_GDAL <- readGDAL(jp2_path)
  sen2_GDAL %>% 
    as.matrix() %>% 
    t() %>% 
    raster(crs=ref_crs, 
           xmn=xmin(sen2_GDAL), xmx=xmax(sen2_GDAL), 
           ymn=ymin(sen2_GDAL), ymx=ymax(sen2_GDAL)) -> tmp_raster
  
  # get band number from file path
  strsplit(jp2_path, "_")[[1]][length(strsplit(jp2_path, "_")[[1]])] %>% 
    strsplit(".jp2") %>% 
    as.character() -> names(tmp_raster)
  
  return(tmp_raster)
}

###name file & set crs
file <- "T32UNA_20190619T103031_B01.tif"#jps
crs_s2 <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
raster <- jps_to_raster(file , crs(crs_s2))

SEN2_PATHS <- list.files(pattern = "*.jp2$", full.names = T)
raster_stack <- raster::stack(lapply(SEN2_PATHS, jp2_to_raster, ref_crs = crs(crs_s2)))

S2_JP2_List <- list.files(jp2_path, full.names = TRUE, pattern = ".jp2$")

### convert into TIF
for (file in S2_JP2_List) {
  out_file <- extension(file, 'tif')
  gdal_translate(src_dataset = file, dst_dataset = out_file, ot =
                   "UInt16", of = "GTiff")
} 
###remove jp2
junk <- dir(path="D:/RS1_2_Fusion/jp2_2", pattern=".jp2$") # 
file.remove(junk) # ?file.remove

###import TIF

TMP_PATH <- paste0(getwd(),"/")

rlist <- paste0(grep("*.tif*", list.files(path = getwd(), pattern="*.tif$", full.names = TRUE), value=T))

Rst_Stk <- lapply(rlist,raster)

crs(Rst_Stk[[2]]) <- CRS(crs_s2)

################################################################ 
################################################################ 




##crop to s1 extent
#tests
# create a raster from the matrix - a "blank" raster of 4x4
myRaster1 <- raster(nrow=10, ncol=10)

# assign "data" to raster: 1 to n based on the number of cells in the raster
myRaster1[]<- 1:ncell(myRaster1)
plot(myRaster1)
extent(myRaster1)
#resampling
Rst_Stk <- resample(myRaster1, file, method='bilinear')

extent(ext_s1)
s2_ext <- crop(Rst_Stk, extent(Rst_Stk, 9.776796 , 10.5206 , 48.97663 , 49.75142 ))

s2_ext <-   alignExtent(Rst_Stk, ext_s1 , snap='near')

alignExtent(ext_s1, Rst_Stk, snap='near')
extent(s1) <- alignExtent(s1, Rst_Stk)

## Resample all to 10x10

for(i in 1: length(Rst_Stk)){
  
  crs(Rst_Stk[[i]]) <- CRS(crs_s2)
  
  if(res(Rst_Stk[[1]]) > 10){
    #resample
    Rst_Stk[[i]] <- resample(Rst_Stk[[2]],Rst_Stk[[i]], method = 'ngb')
    plot(paste0("Raster ", i , " resampled..."))
  }
}

## Stack
Rst_Stk <- stack(Rst_Stk)


for(i in rlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) } 
lapply(rlist, raster)

stack(rlist)

#write the output raster to file
r <- writeRaster(rlist, filename = "D:/RS1_2_Fusion/jp2_2/", format="GTiff", overwrite=TRUE)
##############################################################################################
######################################Reproject###############################################
##############################################################################################

so....

RGB_band2_s2 <- 
  raster("T32UNA_20190619T103031_B02.tif")

plot(RGB_band2_s2)
##############################################################################################
######################################Stacking################################################
##############################################################################################



...in progress^^

