#' Load Exclusive Economic Zones
#'
#' Load Exclusive Economic Zones (EEZs) that fall within the Convention Area
#'
#' @param format "RDATA" will use the Spatial Polygon Data Frame last saved with the package see EEZ_data for further details
#' @keywords Exclusive Economic Zones
#' @import rgeos rgdal raster
#' @export


load_EEZs <-function(format){

  # if(format=="GEOJSON"){
  #   # url that stores the ssurs (this product has not been updated and will not read into R as it doesn't have unique field IDs)
  #   # unique ID issue fixed Jan 2017
  #   ccamlrgisurl<- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:eez&outputFormat=json"
  #   EEZ_data<- readOGR(dsn=ccamlrgisurl,layer="OGRGeoJSON")
  #
  #   return(EEZ_data)
  # }
  if(format=="RDATA"){

    return(CCAMLRGIS::EEZ_data)

  }  
  if (format!="RDATA"){
    warning("only RDATA format is currently available")
  }
  

}
