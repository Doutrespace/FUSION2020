# DATA FUSION DECISION SUPPORT

The FUSION2020 R algorithm based on the 16EAGLES/GetSpatialData package provides automated decision making for finding the most suitable Sentinel-1 and Sentinel-2 image for your research area. The decision is based on the Geometry of both scenes and provides Statistics about the coverage of the scenes above your research area. The algorithm for optical and passive data fusion developed by EAGLE/UniversityofWuerzburg research team is described in more details in this blog.

# Objective: 

The algorithm supports the search for suitable active and passive images for data fusion. Usually a long time is spent to find suitable images for the fusion, because the temporal and spatial axis must be considered. The extension to the package 16Eagles/GetSpatialData developed for the EAGLE program course MB01 at the University of Würzburg, offers the possibility to get an overview of the geometries and relations of active and passive images in relation to the initially determined polygons of the research area.

## **1.Data Extraction**

The data is loaded via the API of ESA [scihub.copernicus] ( https://scihub.copernicus.eu/ ) using the 16Eagles/GetSpatialData package and then filtered by our algorithm.

## **Workflow**

The first step is to determine the area of investigation. Then the log-in data for the scihub.copernicus of ESA (https://scihub.copernicus.eu/) are entered. The next step determines the date at which the existing data is to be searched ("Enter start / end of timespan (YYYY-MM-DD). Then the algorithm can run through to carry out its analysis.

# Data frame output (Statistics)

The Match_Df #in ViewMatch, which will give you the following insights:

| S1_ID| S1_Date   | S2_ID |S2_Date    |DateDiff |S1S2OV |S1S2D  | S1AD  | S2AD   | AIID | 
| ---- | -------   | ----- | --------- | -----   | ----- | ----- | ----- | ------ | ---- | 
| 2    |2020-03-30 | 2     |2020-03-29 | 1       | TRUE  | 15.11 | 0     | 78.87  |82.56 |
| 5    |2020-04-02 | 3     |2020-03-30 | 2       | TRUE  | 14.36 | 16.78 | 14.89  |56.87 |
| 9    |2020-04-08 | 7     |2020-04-01 | 4       | TRUE  | 65.78 | 89.76 |77.34   |100   | 

S1_Date = Pre defined S1 date (time in between)                      
S2_Date = Pre defined S2 date (time in between)
Date_Diff = Time difference between S1 and S2 in days
S1S2OV = AOI is lying in the S1 and S2 area (TRUE/FALSE)
S1S2D =
S1AD =
AIID = 


<p align="center">
  <img width="460" height="300" src="https://github.com/Doutrespace/FUSION2020/blob/master/Image/Overview01.PNG">
</p>


The figure shows us how Sentinel 1 and Sentinel 2 are related to our AOI.





<p align="center">
  <img width="460" height="300" src="https://github.com/Doutrespace/FUSION2020/blob/master/Image/Overview02.PNG">
</p>






<p align="center">
  <img width="460" height="300" src="https://github.com/Doutrespace/FUSION2020/blob/master/Image/Overview_esa.PNG">
</p>









<p align="center">
  <img width="460" height="300" src="https://github.com/Doutrespace/FUSION2020/blob/master/Image/overview_esa_micro.PNG">
</p>



## **Outlook:**


Furthermore, a cloud mask of the optical image is added to determine the percentage cloud content in the polygon of the study area. This enables the user to use the images which have been filtered out of the search by an increased cloud content. 
