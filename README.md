# DATA FUSION DECISION SUPPORT

The FUSION2020 R algorithm based on the 16EAGLES/GetSpatialData package provides automated decision making for finding the most suitable Sentinel-1 and Sentinel-2 image for your research area. The decision is based on the Geometry of both scenes and provides Statistics about the coverage of the scenes above your research area. The algorithm for optical and passive data fusion developed by EAGLE/UniversityofWuerzburg research team is described in more details in this blog.

# Objective: 

The algorithm supports the search for suitable active and passive images for data fusion. Usually a long time is spent to find suitable images for the fusion, because the temporal and spatial axis must be considered. The extension to the packet 16Eagles/GetSpatialData developed at the University of Würzburg offers the possibility to get an overview of the geometries and relations of active and passive images in relation to the initially determined of polygons the research area in a few steps.

## **1.Data Extraction**

The data is loaded via the API of ESA [scihub.copernicus] ( https://scihub.copernicus.eu/ ) using the 16Eagles/GetSpatialData package and then filtered by our algorithm.

## **Workflow**

The first step is to determine the area of investigation. Then the log-in data for the scihub.copernicus of ESA (https://scihub.copernicus.eu/) are entered. The next step determines the date at which the existing data is to be searched ("Enter start / end of timespan (YYYY-MM-DD). Then the algorithm can run through to carry out its analysis.

# Data frame output (Statistics)

Match_Df #in ViewMatch in viewer, which will look like this:
```

<p align="center"><img width="60%" src="image/Overview01.PNG"></p>
<p align="center"><sub>Figure 1: Screenshot of the RStudio Viewer, displaying the previously defined session AOI and Sentinel-1 and Sentinel-2 data of your choice using ViewMatch</sub></p>
<br>




## **Outlook:**


Furthermore, a cloud mask of the optical image is added to determine the percentage cloud content in the polygon of the study area. This enables the user to use the images which have been filtered out of the search by an increased cloud content. 
