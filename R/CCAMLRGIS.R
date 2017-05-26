#' CCAMLRGIS: An R package for loading and creating geographic data that is compatible with the online CCAMLR GIS
#'
#' The CCAMLRGIS package provides two categories of functions: load functions and create functions 
#' 
#' @section load functions are used to import CCAMLR Reference into R (ideally from the geoserver) and include:
#' load_ASDs, load_SSRUs, load_RBs, load_SSMUs, load_MAs and load_EEZs provide easy loading of CCAMLR reference data 
#' Data that is classified as "master" data is available in "GeoJSON" format and "Rdata" format is available in all load functions 
#' 
#' @section create functions are used to create spatial data that is compatible with CCAMLR Reference data and the CCAMLR online GIS. The include:
#'  create_Polys, create_Points, create_Lines, create_PolyGrids
#'  Other functions that support the create functions include Clip2Coast and DensifyData
#'  
#' @docType package
#' @name CCAMLRGIS
NULL

#' ASD data
#'  
#' A SpatialPolygonsDataFrame. 
#' @docType data
#' @name ASD_data
NULL


#' MPA data
#'  
#' A SpatialPolygonsDataFrame. 
#' @docType data
#' @name MPA_data
NULL

#' RB data
#'  
#' A SpatialPolygonsDataFrame. 
#' @docType data
#' @name RB_data
NULL

#' RefAreas 
#'  
#' A SpatialPolygonsDataFrame. 
#' @docType data
#' @name RefAreas
NULL

