############################################################################################################
##################################### S1&S2_Fusion #########################################################
# Code made by: Nils Karges (s338495)
# Email: nils.karges@stud-mail.uni-wuerzburg.de
############################################################################################################
############################################################################################################
#devtools::install_github("16EAGLE/getSpatialData")
#Antonio
#Fusion_Folder <-  "C:/Users/Cowboybebop/Documents/EAGLE/0_Other/Additional_Projects/FUSION2020"
#Nils
Fusion_Folder <- "D:/FUSION2020" 
setwd(Fusion_Folder)
### Import Functions from Function.R File
source("Functions.R")

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

# usage
packages <- c("sp","raster","rlist","getSpatialData","sf","sp","list","rSNAP","processx","dplyr","stringi",
              "installr","lubridate","rgdal","data.table","devtools","svDialogs","gdalUtils","Rcpp", "mapview",
              "mapedit","stringr","rgeos","rlang","officer","shiny","flextable","maps","mapproj","ggplot2","Orcs",
              "tryCatchLog")
ipak(packages)


###call set_aoi() without argument, which opens a mapedit editor:
area <- mapview(editMap())
area <- area@object[[1]][[1]][[1]]
area <- Polygon(area)
area <- Polygons(c(area), "area")
area <- SpatialPolygons(c(area), c(1:1), proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))

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
Sentinel_2 <- getSentinel_query(time_range = c(TimeSpan_Start , TimeSpan_End), platform = "Sentinel-2", aoi = area) #or "Sentinel-1" or "Sentinel-3"
Sentinel_1 <- getSentinel_query(time_range = c(TimeSpan_Start , TimeSpan_End), platform = "Sentinel-1", aoi = area) #or "Sentinel-1" or "Sentinel-3"

### Filter the records
###see all available filter attributes
unique(Sentinel_2$processinglevel)

Sentinel_2_filtered <- Sentinel_2[which(Sentinel_2$processinglevel == "Level-2A"),] #filter by Level
Sentinel_2_filtered <- Sentinel_2_filtered[as.numeric(Sentinel_2_filtered$cloudcoverpercentage) <= 30, ] #filter by clouds
Sentinel_1_filtered <- Sentinel_1[which(Sentinel_1$producttype == "GRD"),] #format

########################################################################################
############################   Find the Fusion pairs   #################################
########################################################################################

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
  #print(Tem_List)
  
  ### Get the row.name min val in each list
  Temporal_Min <- min(Sentinel_2_filtered$cloudcoverpercentage[Tem_List])
  
  ### produces temporal index
  Temporal_Index <- which(Temporal_Min == Sentinel_2_filtered$cloudcoverpercentage[Tem_List], arr.ind=T)
  
  ### index ###############################?
  #Index <- Temporal_Index[[1]][1]
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

###########################################################################################
### Make comparison Day timespan between S2 and S1 (S2 FIRST BECAUSE IS LESS DATA) #
###########################################################################################

###Finding the closest Date
Sentinel_1_filtered$ClosestS2Date <- 0

# Connect the ID?s
Final_S1_ID <- Sentinel_1_filtered$ID

# Temporal ID for S2
Final_S2_ID <- Sentinel_1_filtered$ID

###
for(i in 1:length(Sentinel_1_filtered$ingestiondate)){
  date <- as.Date(Sentinel_1_filtered$ingestiondate[i])
  Final_S2_ID[i] <-  Sentinel_2_filtered$ID[which.min(abs(date - as.Date(Sentinel_2_filtered$ingestiondate)))]
}

Sentinel_1 <- Sentinel_1[Final_S1_ID,]
Sentinel_2 <- Sentinel_2[unique(Final_S2_ID),]



#Sentinel_1_filtered$Overlap <- 0
#Sentinel_1_filtered$Overlap <- sapply(Sentinel_1_filtered$footprint, function(x) MFPF(x))

#Match_df$S2Ov <- sapply(Sentinel_2$footprint[Match_df$S2_ID], function(x) MFPF(x))


######################################################################################## 
###########################Match Dataframe##############################################
########################################################################################

# Build match dataframe to construct the GUI
Match_df <- as.data.frame(Final_S1_ID, stringsAsFactors = FALSE)
Match_df$S1_Date <- as.Date(Sentinel_1_filtered$ingestiondate)
Match_df$S2_ID <- as.integer(Final_S2_ID)

#-> 8 has to be replaced to match the col name "ingestiondate", in my case is 9 thats why! ################################ solved
Match_df$S2_Date <- as.Date(substr(Sentinel_2[Final_S2_ID,grep("ingestiondate", colnames(Sentinel_2))],1,10))
# setting row names
names(Match_df) <- c("S1_ID","S1_Date","S2_ID","S2_Date")
# set match df integer
Match_df$S1_ID <- as.integer(Final_S1_ID)

Match_df$DateDiff <- abs(difftime(Match_df$S1_Date, Match_df$S2_Date , units = c("days")))

Match_df$S1S2D <- 0
Match_df$S1AD <- 0
Match_df$S2AD <- 0
Match_df$AllD <- 0

#---------------> INCEPTION <------------------------------------
for(i in 1: length(Match_df$S1_ID)){
  
  TempData <- PolOv(Char2Pol(Sentinel_1$footprint[Match_df$S1_ID[i] == row.names(Sentinel_1)],),
                    Char2Pol(Sentinel_2$footprint[Match_df$S2_ID[i] == row.names(Sentinel_2)],),
                    area)
  
  # revisar cómo se calculan las áreas y que pasa cuando AOI es más grande que el área de S2
  # preguntar al comienzo por la aplicacion, quiere aplicar el filtro de nuves? si o no 
  
  Match_df$S1S2D[i]  <- TempData[1]
  Match_df$S1AD[i]   <- TempData[2]
  Match_df$S2AD[i]   <- TempData[3]
  Match_df$AllD[i]   <- TempData[4]
  
}

ViewMatch(Char2Pol(Sentinel_1$footprint[3],"S1"),Char2Pol(Sentinel_2$footprint[3],"S2"),area)####### Antonio should fix it
########################################################################################
######################################################################################## 
########################################################################################
########################################################################################




Test1 <- Char2Pol(Sentinel_1$footprint[2],"S1")
Test2 <- Char2Pol(Sentinel_2$footprint[1],"S2")

Dataframetst <- as.data.frame("Poligons" = c(Test1,Test2),"ID" =c("S2","S1"))

Test1@polygons[[1]]@ID
Test2@polygons[[1]]@ID

List_Test <- list(Test1, Test2, area)

plot(Test1@polygons@Polygons)

SpatialPolygonsDataFrame(List_Test[1], "Id")


mapview(List_Test, zcol = "ID")# c(List_Test[[1]]@polygons[[1]]@ID,List_Test[[2]]@polygons[[1]]@ID,List_Test[[2]]@polygons[[1]]@ID))

# Option2 -> build a dataframe 



############## IF AREA OF S§ < AOI THEN; DELETE THOSE FROM MATCHDF AND CHANGE THE S" POLIGON COLOR IN GUI
############## TO RED AND PUT A WARNING
######################################################################################## 
########################################################################################
######################################################################################## 
########################################################################################
########################################################################################

Sentinel_1$footprint[Match_df$S1_ID[i] == row.names(Sentinel_1)]

TempData <- PolOv(Char2Pol(Sentinel_1$footprint[row.names(Sentinel_1) == Match_df$S1_ID[1]]),
                  Char2Pol(Sentinel_2$footprint[row.names(Sentinel_2) == Match_df$S2_ID[1]]),
                  area)

TempData[2]
  

Sentinel_1$footprint[row.names(Sentinel_1) == Match_df$S1_ID]

row.names(Sentinel_1)

Match_df$S1_ID


#Final@plotOrder
#######################

<<<<<<< HEAD
###############
  
str_remove_all(MP_Test, "[()MULTIPOLYGON]")


Tmp <- strsplit(Tmp,",")
Tmp <- strsplit(Tmp[[1]]," ")
df <- data.frame(Long=double(),Lat=double()) 



########










=======
>>>>>>> b1535583bffd9e0a4e67f4f0e8ee48563f5e05b8





class(Sentinel_2$footprint[Match_df$S2_ID == rownames(Sentinel_2)])



Sentinel_2$footprint[rownames(Sentinel_2) == Final_S2_ID]





# Check unique dates in case there is only one image available 
Checker <- function(S1_ID, S2_ID){
  if(length(unique(S1_ID))<= 1 | length(unique(S2_ID))<=1 ){
    if(ask.user.yn.question(" There is only one S1 imagery with the given criteria
                              available, to have better results you are able to
                              apply 5 DateDiff filter. Do you want to do
                              it?")){
      # Here a "DATE" filter can be made! (if DateDiff <= 5 days then delete row)
      Match_df <<- subset(Match_df, DateDiff <= 5)
      print("Filter applied")} else{|
      print("Filter not applied")
      }
  }  else{
    print("Well Done")
  }
}


Checker(Final_S1_ID,Final_S2_ID)
########################################################################################
########################################################################################



########################################################################################
################################### Download the Data ##################################
########################################################################################

## Download some datasets to your archive directory
#datasets <- getSentinel_data(records =  Sentinel_2[unique(Final_S2_ID), ])
#datasets <- getSentinel_data(records =  Sentinel_1[unique(Final_S1_ID), ])

DownloadList <- c(1)

datasets <- getSentinel_data(records =  Sentinel_1[row.names(Sentinel_1) == Match_df$S1_ID[DownloadList],] , dir_out = Archive_Folder)
datasets <- getSentinel_data(records =  Sentinel_2[row.names(Sentinel_2) == Match_df$S2_ID[DownloadList],] , dir_out = Archive_Folder)


#datasets <- getSentinel_data(records = records_filtered[c(4,7,9), ])

## Finally, define an output format and make them ready-to-use
datasets <- prepSentinel(datasets, format = "tiff", dir_out = Archive_Folder)
# or use VRT to not store duplicates of different formats
datasets_prep <- prepSentinel(datasets, format = "vrt")

## View the files
datasets_prep[[1]][[1]][1] #first dataset, first tile, 10 m resolution
datasets_prep[[1]][[1]][2] #first dataset, first tile, 20 m resolution
datasets_prep[[1]][[1]][3] #first dataset, first tile, 60 m resolution

## Load them directly into R
r <- stack(datasets_prep[[1]][[1]][1])

########################################################################################
########################################################################################
########################################################################################



