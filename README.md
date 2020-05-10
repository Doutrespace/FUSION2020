![](https://img.shields.io/badge/EAGLE-Design-blue) ![](https://img.shields.io/badge/Approval-pending-red)
# DATA FUSION DECISION SUPPORT

The FUSECISIONR algorithm based on the 16EAGLES/GetSpatialData package provides automated decision making for finding the most suitable Sentinel-1 and Sentinel-2 images for your research area. The decision is based on the Geometry of both scenes and provides Statistics about the coverage of the scenes above your research area. The algorithm which serves as a decision support for the selection of optical and passive images for data fusion purposes developed by EAGLE/UniversityofWuerzburg first semester Msc students is described in more details in this blog.

## Objective

The algorithm supports the search for suitable active and passive images for data fusion. Usually a long time is spent to find suitable images for the fusion, as not only the cloud cover or the orbit direction is decisive but also the ideal spatial and temporal axis must be considered. The extension to the package 16Eagles/GetSpatialData developed for the EAGLE program course MB02 at the University of Würzburg, offers the possibility to get an overview of the geometries and relations of active and passive images in relation to the initially determined polygons of the research area.
The data is loaded via the API of ESA Scihub.Copernicus ( https://scihub.copernicus.eu/ ) using the 16Eagles/GetSpatialData package and then filtered by our algorithm.

## **Workflow**

The first step is to determine the area of investigation. Then the log-in data for the scihub.copernicus of ESA (https://scihub.copernicus.eu/) are entered. The next step determines the date at which the existing data is to be searched ("Enter start / end of timespan (YYYY-MM-DD). The data is loaded via the API of ESA [scihub.copernicus] ( https://scihub.copernicus.eu/ ) using the 16Eagles/GetSpatialData package and then filtered by our algorithm.

## **Data frame output (Statistics)**

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




ViewMatch overview           | ViewMatch centred on sentinel 2 
:-------------------------:|:-------------------------:
![](https://github.com/Doutrespace/FUSION2020/blob/master/Image/Overview01.PNG)  |  ![](https://github.com/Doutrespace/FUSION2020/blob/master/Image/Overview02.PNG)

The figure shows us how Sentinel 1 and Sentinel 2 are related to our AOI.






Information from ESA          | ESA centred on sentinel 2 
:-------------------------:|:-------------------------:
![](https://github.com/Doutrespace/FUSION2020/blob/master/Image/Overview_esa.PNG)  |  ![](https://github.com/Doutrespace/FUSION2020/blob/master/Image/overview_esa_micro.PNG)


Since you don't get information from ESA about how much of the data spans the research area, our tool can help you make the right decisions.



## **Outlook**


Furthermore, a cloud mask of the optical image is added to determine the percentage cloud content in the polygon of the study area. This enables the user to use the images which have been filtered out of the search by an increased cloud content. 
