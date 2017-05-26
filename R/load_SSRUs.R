#' load CCAMLR SSRUs
#'
#' Load CCAMLR Small Scale Research Units (SSRUs)
#'
#' @param format two different formats can be specified. To extract the latest SSRU geographical reference data from the CCAMLR GIS website enter "GEOJSON" and to use the Spatial Polygon Data Frame last saved with the package enter "RDATA" see ?SSRU_data for further details/documentation
#' @keywords SSRU Small Scale Research Units
#' @import rgeos rgdal raster
#' @export


load_SSRUs <-function(format){

  if(format=="GEOJSON"){
  # url that stores the ssurs (this product has not been updated and will not read into R as it doesn't have unique field IDs)
  # unique ID issue fixed Jan 2017
  ccamlrgisurl<- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:ssrus&outputFormat=json"
  SSRU_data<- readOGR(dsn=readLines(ccamlrgisurl, warn=F),layer="OGRGeoJSON",verbose = FALSE)
  
  return(SSRU_data)
  }
  if(format=="RDATA"){

  return(SSRU_data)

  }
  if (!format%in%c("RDATA","GEOJSON")){
    warning("only RDATA or GEOJSON format is available")
  }
}

