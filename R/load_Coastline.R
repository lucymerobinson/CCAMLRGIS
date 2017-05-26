#' load CCAMLR Coastline
#'
#' Load CCAMLR Coastline based on SCAR Coastline data 
#'
#' @param format two different formats can be specified. To extract the latest Coastline reference data from the CCAMLR GIS website enter "GEOJSON" and to use the Spatial Polygon Data Frame last saved with the package enter "RDATA" see ?Coastline_data for further details/documentation
#' @keywords Coastline 
#' @import rgeos rgdal raster
#' @export


load_Coastline <-function(format){
  
  if(format=="GEOJSON"){
    # url that stores the ssurs (this product has not been updated and will not read into R as it doesn't have unique field IDs)
    # unique ID issue fixed Jan 2017
    ccamlrgisurl<- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:coastline&outputFormat=json"
    Coastline_data<- readOGR(dsn=readLines(ccamlrgisurl, warn=F),layer="OGRGeoJSON",verbose = FALSE)
    
    return(Coastline_data)
  }
  if(format=="RDATA"){
    
    return(Coastline_data)
    
  }
  if (!format%in%c("RDATA","GEOJSON")){
    warning("only RDATA or GEOJSON format is available")
  }
}

