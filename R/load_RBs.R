#' Load CCAMLR Research Blocks
#'
#' Load CCAMLR Research Blocks (RBs)
#'
#' @param format "GEOJSON" will extract the latest Research Block geographical reference data from the CCAMLR GIS website and "RDATA" will use the Spatial Polygon Data Frame last saved with the package
#' @keywords Research Blocks
#' @import rgeos rgdal raster
#' @export


load_RBs <-function(format){

  if(format=="GEOJSON"){
    # url that stores the ssurs (this product has not been updated and will not read into R as it doesn't have unique field IDs)
    # unique ID issue fixed Jan 2017
    ccamlrgisurl<- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:research_blocks&maxFeatures=50&outputFormat=json"
    RB_data<- readOGR(dsn=readLines(ccamlrgisurl, warn=F),layer="OGRGeoJSON",verbose = FALSE)
    
    return(RB_data)
  }
  if(format=="RDATA"){

    return(RB_data)

  }
  if (!format%in%c("RDATA","GEOJSON")){
    warning("only RDATA or GEOJSON format is available")
  }

}

