#' CCAMLRGIS: An R package for loading and creating geographic data that is compatible with the online CCAMLR GIS
#'
#' The CCAMLRGIS package provides two categories of functions: load functions and create functions 
#' 
#' @section load functions are used to import CCAMLR Reference into R (ideally from the geoserver) and include:
#' load_ASDs, load_SSRUs, load_RBs, load_SSMUs, load_MAs, load_Coastline, load_RefAreas and load_MPAs provide easy loading of CCAMLR Reference data 
#' Data that is classified as "master" data is available in "GEOJSON" format and "RDATA" format is available in all load functions 
#' 
#' @section create functions are used to create spatial data that is compatible with CCAMLR Reference data and the CCAMLR online GIS. The include:
#'  create_Polys and create_PolyGrids
#'  Other functions that support the create functions include Clip2Coast and DensifyData
#'  
#' @docType package
#' @name CCAMLRGIS
#' @importFrom graticule graticule
NULL

#' ASD data
#'  
#' @docType data
#' @format SpatialPolygonsDataFrame
#' @source https://gis.ccamlr.org/home see information (i) under Management areas
#' @name ASD_data
NULL

#' SSRU data
#'  
#' @docType data
#' @format SpatialPolygonsDataFrame
#' @source https://gis.ccamlr.org/home see information (i) under Management areas
#' @name SSRU_data
NULL

#' SSMU data
#'  
#' @docType data
#' @format SpatialPolygonsDataFrame
#' @source https://gis.ccamlr.org/home see information (i) under Management areas
#' @name SSMU_data
NULL


#' Coastline data
#'  
#' @docType data
#' @format SpatialPolygonsDataFrame
#' @source https://gis.ccamlr.org/home see information (i) under Coastlines
#' @name Coastline_data
NULL

#' MA data
#'  
#' @docType data
#' @format SpatialPolygonsDataFrame
#' @source https://gis.ccamlr.org/home see information (i) under Management areas
#' @name MA_data
NULL


#' MPA data
#'  
#' @docType data
#' @format SpatialPolygonsDataFrame
#' @source https://gis.ccamlr.org/home see information (i) under Management areas
#' @name MPA_data
NULL


#' RB data
#'  
#' @docType data
#' @format SpatialPolygonsDataFrame
#' @source https://gis.ccamlr.org/home see information (i) under Management areas
#' @name RB_data
NULL

#' RefAreas 
#'  
#' @docType data
#' @format SpatialPolygonsDataFrame
#' @name RefAreas
NULL


#' EEZ data
#'  
#' @docType data
#' @format SpatialPolygonsDataFrame
#' @source https://gis.ccamlr.org/ see information (i) under Management areas
#' @name EEZ_data
NULL




