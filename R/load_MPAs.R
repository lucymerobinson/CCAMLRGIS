#' load CCAMLR MPAs
#'
#' Load CCAMLR Marine Protected Areas (MPAs)
#'
#' @param format two different formats can be specified. To extract the latest MPA geographical reference data from the CCAMLR GIS website enter "GEOJSON" and to use the Spatial Polygon Data Frame last saved with the package enter "RDATA" see ?MPA_data for further details/documentation
#' @keywords Marine Protected Areas
#' @import rgeos rgdal raster
#' @export


load_MPAs <-function(format){
  
  if(format=="GEOJSON"){
    
    ccamlrgisurl<- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:mpas&outputFormat=json"
   
    
     MPA_data<- readOGR(dsn=readLines(ccamlrgisurl, warn=F),layer="OGRGeoJSON",verbose = FALSE)
    
    return(MPA_data)
  }
  if(format=="RDATA"){
    
    return(MPA_data)
    
  }
  if (!format%in%c("RDATA","GEOJSON")){
    warning("only RDATA or GEOJSON format is available")
  }
}

