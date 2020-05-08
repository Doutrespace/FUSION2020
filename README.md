# **DATA FUSION DECISION SUPPORT**

The FUSION2020 R algorithm based on the 16EAGLES/GetSpatialData package provides automated decision making for finding the most suitable Sentinel-1 and Sentinel-2 image for your research area. The decision is based on the Geometry of both scenes and provides Statistics about the coverage of the scenes above your research area. The algorithm for optical and passive data fusion developed by EAGLE/UniversityofWuerzburg research team is described in more details in this blog.


## **Objective**

The algorithm supports the search for suitable active and passive images for data fusion. Usually a long time is spent to find suitable images for the fusion, because the temporal and spatial axis must be considered. The extension to the packet 16Eagles/GetSpatialData developed at the University of Würzburg offers the possibility to get an overview of the geometries and relations of active and passive images in relation to the initially determined of polygons the research area in a few steps.

## **Outlook:**

Furthermore, a cloud mask of the optical image is added to determine the percentage cloud content in the polygon of the study area. This enables the user to use the images which have been filtered out of the search by an increased cloud content. 
