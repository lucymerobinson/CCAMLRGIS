
#' Load CCAMLR Statistical Areas and Divisions
#'
#' Load CCAMLR Statistical Areas and Divisions (ASDs)
#'
#' @param format "GEOJSON" will extract this geographical reference data displayed on the CCAMLR GIS website and "RDATA" will use the Spatial Polygon Data Frame last saved with the package
#' @keywords Statistical Areas and Divisions
#' @import rgeos rgdal raster
#' @importFrom utils download.file
#' @export
#' @examples  
#' # if online
#' \dontrun{
#'   ASDs <- load_ASDs("GEOJSON")
#' }
#' # if offline 
#' ASDs <- load_ASDs("RDATA")
load_ASDs <- function(format){
  
  if(format=="GEOJSON"){

    ccamlrgisurl<- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:statistical_areas&outputFormat=json"
    temp <- sprintf("%s.json", tempfile())
    utils::download.file(ccamlrgisurl, temp, mode = "wb")
    ASD_data<- readOGR(dsn=temp,verbose = FALSE)
    return(ASD_data)
  }
  if(format=="RDATA"){
    
    return(ASD_data)
  }
  if (!format%in%c("RDATA","GEOJSON")){
    warning("only RDATA or GEOJSON format is available")
  }
  
}


#' load CCAMLR SSRUs
#'
#' Load CCAMLR Small Scale Research Units (SSRUs)
#'
#' @param format "GEOJSON" will extract this geographical reference data displayed on the CCAMLR GIS website and "RDATA" will use the Spatial Polygon Data Frame last saved with the package
#' @keywords SSRU Small Scale Research Units
#' @import rgeos rgdal raster sf
#' @importFrom methods as
#' @export
#' @examples  
#' # if online
#' \dontrun{
#' SSRUs <- load_SSRUs("GEOJSON")
#' }
#' # if offline 
#' SSRUs <- load_SSRUs("RDATA")
load_SSRUs <-function(format){
  
  if(format=="GEOJSON"){

    ccamlrgisurl<- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:ssrus&outputFormat=json"
    # SSRU_data<- suppressWarnings(readOGR(dsn=ccamlrgisurl,layer="OGRGeoJSON",verbose = FALSE))
    SSRU_data <- methods::as(sf::read_sf(ccamlrgisurl),"Spatial")
    
    return(SSRU_data)
  }
  if(format=="RDATA"){
    
    return(SSRU_data)
    
  }
  if (!format%in%c("RDATA","GEOJSON")){
    warning("only RDATA or GEOJSON format is available")
  }
}


#' load CCAMLR Coastline
#'
#' Load CCAMLR Coastline based on SCAR Coastline data 
#'
#' @param format "RDATA" will use the SpatialPolygonsDataFrame last saved with the package
#' @keywords Coastline 
#' @import rgeos rgdal raster
#' @export
#' @examples  
#' 
#' # if offline 
#' Coast <- load_Coastline("RDATA")

load_Coastline <-function(format){
  
  if(format=="GEOJSON"){
    
    # cat("data currently not available on the CCAMLR online GIS")
    ccamlrgisurl <- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:coastline&outputFormat=json"
    Coastline_data <- readOGR(dsn=ccamlrgisurl,layer="OGRGeoJSON",verbose = FALSE)
    
    # return(Coastline_data)
  }
  if(format=="RDATA"){
    
    return(CCAMLRGIS::Coastline_data)
    
  }
  if (!format%in%c("RDATA","GEOJSON")){
    warning("only RDATA or GEOJSON format is available")
  }
}




#' Load CCAMLR Research Blocks
#'
#' Load CCAMLR Research Blocks (RBs)
#'
#' @param format "GEOJSON" will extract this geographical reference data displayed on the CCAMLR GIS website and "RDATA" will use the Spatial Polygon Data Frame last saved with the package
#' @keywords Research Blocks
#' @import rgeos rgdal raster
#' @importFrom methods as
#' @export
#' @examples  
#' # if online
#' \dontrun{
#' RBs <- load_RBs("GEOJSON")
#' }
#' 
#' # if offline 
#' RBs <- load_RBs("RDATA")

load_RBs <-function(format){
  
  if(format=="GEOJSON"){

    ccamlrgisurl<- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:research_blocks&maxFeatures=50&outputFormat=json"
    # RB_data<- suppressWarnings(readOGR(dsn=ccamlrgisurl,layer="OGRGeoJSON",verbose = FALSE))
    RB_data <- methods::as(sf::read_sf(ccamlrgisurl),"Spatial")
    
    return(RB_data)
  }
  if(format=="RDATA"){
    
    return(RB_data)
    
  }
  if (!format%in%c("RDATA","GEOJSON")){
    warning("only RDATA or GEOJSON format is available")
  }
  
}

#' Load CCAMLR Small Scale Management Units
#'
#' Load CCAMLR Small Scale Management Units (SSMUs)
#'
#' @param format "GEOJSON" will extract this geographical reference data displayed on the CCAMLR GIS website and "RDATA" will use the Spatial Polygon Data Frame last saved with the package
#' @keywords Small Scale Management Units (SSMUs)
#' @import rgeos rgdal raster sf
#' @importFrom methods as
#' @export
#' @examples  
#' # if online
#' \dontrun{
#' SSMUs <- load_SSMUs("GEOJSON")
#' }
#' 
#' # if offline 
#' SSMUs <- load_SSMUs("RDATA")
load_SSMUs <-function(format){
  
  if(format=="GEOJSON"){

    ccamlrgisurl<- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:ssmus&outputFormat=json"
    # SSMU_data <- suppressWarnings(readOGR(dsn=ccamlrgisurl,layer="OGRGeoJSON",verbose=FALSE))
    SSMU_data <- methods::as(sf::read_sf(ccamlrgisurl),"Spatial")
    return(SSMU_data)
  }
  if(format=="RDATA"){
    return(CCAMLRGIS::SSMU_data)
  }
  if (!format%in%c("RDATA","GEOJSON")){
    warning("only RDATA or GEOJSON format is available")
  }
}


#' Load Other Fishery Management Areas

#' Load Other Fishery Management Areas
#'
#' @param format "GEOJSON" will extract this geographical reference data displayed on the CCAMLR GIS website and "RDATA" will use the Spatial Polygon Data Frame last saved with the package
#' @import rgeos rgdal raster sf
#' @importFrom methods as
#' @export
#' @examples  
#' # if online
#' \dontrun{
#' MAs <- load_MAs("GEOJSON")
#' }
#' # if offline 
#' MAs <- load_MAs("RDATA")
load_MAs <-function(format){
  
  if(format=="GEOJSON"){

    ccamlrgisurl<- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:omas&outputFormat=json"
    # MA_data<- suppressWarnings(readOGR(dsn=ccamlrgisurl,layer="OGRGeoJSON",verbose = FALSE))
    MA_data <- methods::as(sf::read_sf(ccamlrgisurl),"Spatial")
    return(MA_data)
  }
  if(format=="RDATA"){
    
    return(MA_data)
    
  }
  if (!format%in%c("RDATA","GEOJSON")){
    warning("only RDATA or GEOJSON format is available")
  }
}


#' Load CCAMLR Reference Areas
#'
#' Load CCAMLR Reference Areas (RefAreas)
#'
#' @param format  "RDATA" will use the SpatialPolygonsDataFrame last saved with the package
#' @keywords Reference areas
#' @import rgeos rgdal raster
#' @export
#' @examples  
#' RefAreas <- load_RefAreas("RDATA")

load_RefAreas <-function(format){
  
  if(format=="GEOJSON"){
    
    cat("data currently not available on the CCAMLR online GIS")
  }
  if(format=="RDATA"){
    
    return(CCAMLRGIS::RefAreas)
    
  }
  if (!format%in%c("RDATA","GEOJSON")){
    warning("only RDATA or GEOJSON format is available")
  }
  
}


#' load CCAMLR MPAs
#'
#' Load CCAMLR Marine Protected Areas (MPAs)
#'
#' @param format "GEOJSON" will extract this geographical reference data displayed on the CCAMLR GIS website and "RDATA" will use the Spatial Polygon Data Frame last saved with the package
#' @keywords Marine Protected Areas
#' @import rgeos rgdal raster sf
#' @importFrom methods as
#' @export

load_MPAs <-function(format){
  
  if(format=="GEOJSON"){
    
    ccamlrgisurl<- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:mpas&outputFormat=json"
    # MPA_data<- suppressWarnings(readOGR(dsn=ccamlrgisurl,layer="OGRGeoJSON",verbose = FALSE))
    MPA_data <- methods::as(sf::read_sf(ccamlrgisurl),"Spatial")
    return(MPA_data)
  }
  if(format=="RDATA"){
    
    return(MPA_data)
    
  }
  if (!format%in%c("RDATA","GEOJSON")){
    warning("only RDATA or GEOJSON format is available")
  }
}


#' Load Exclusive Economic Zones
#'
#' Load Exclusive Economic Zones (EEZs) that fall within the Convention Area
#'
#' @param format "GEOJSON" will extract this geographical reference data displayed on the CCAMLR GIS website and "RDATA" will use the Spatial Polygon Data Frame last saved with the package
#' @keywords Exclusive Economic Zones
#' @import rgeos rgdal raster sf
#' @importFrom methods as
#' @export

load_EEZs <-function(format){
  if(format=="GEOJSON"){
    ccamlrgisurl<- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:eez&outputFormat=json"
    # EEZ_data<- suppressWarnings(readOGR(dsn=ccamlrgisurl,layer="OGRGeoJSON",verbose = FALSE))
    EEZ_data <- methods::as(sf::read_sf(ccamlrgisurl),"Spatial")
    return(EEZ_data)
  }
  if(format=="RDATA"){
    return(EEZ_data)
  }  
}
