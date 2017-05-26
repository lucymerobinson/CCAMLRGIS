#' Load CCAMLR Small Scale Management Units
#'
#' Load CCAMLR Small Scale Management Units (SSMUs)
#'
#' @param format "RDATA" will use the Spatial Polygon Data Frame last saved with the package see ?SSMU_data for documentation of data/further details
#' @keywords Small Scale Management Units (SSMUs)
#' @import rgeos rgdal raster
#' @export



load_SSMUs <-function(format){

  # if(format=="GEOJSON"){
  #   # url that stores the ssurs (this product has not been updated and will not read into R as it doesn't have unique field IDs)
  #   # unique ID issue fixed Jan 2017
  #   ccamlrgisurl<- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:management_areas&outputFormat=json"
  #   MA_data<- readOGR(dsn=ccamlrgisurl,layer="OGRGeoJSON")
  #
  #   return(MA_data)
  # }
  if(format=="RDATA"){
    return(SSMU_data)
  }
  if (format!="RDATA"){
    warning("only RDATA format is currently available")
  }

}

