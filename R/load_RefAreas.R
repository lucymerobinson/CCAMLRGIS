#' Load CCAMLR Reference Areas
#'
#' Load CCAMLR Reference Areas (RefAreas)
#'
#' @param format "GEOJSON" will extract the latest Research Block geographical reference data from the CCAMLR GIS website and "RDATA" will use the Spatial Polygon Data Frame last saved with the package
#' @keywords Reference areas
#' @import rgeos rgdal raster
#' @export


load_RefAreas <-function(format){
  
  if(format=="GEOJSON"){
    # # url that stores the ssurs (this product has not been updated and will not read into R as it doesn't have unique field IDs)
    # # unique ID issue fixed Jan 2017
    # ccamlrgisurl<- "https://gis.ccamlr.org/geoserver/gis/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=gis:research_blocks&maxFeatures=50&outputFormat=json"
    # RB_data<- readOGR(dsn=readLines(ccamlrgisurl, warn=F),layer="OGRGeoJSON",verbose = FALSE)
    # 
    # return(RB_data)
    cat("data currently not available on the CCAMLR online GIS")
  }
  if(format=="RDATA"){
    
    return(RefAreas)
    
  }
  if (!format%in%c("RDATA","GEOJSON")){
    warning("only RDATA or GEOJSON format is available")
  }
  
}
# 
# library(rgdal)
# setwd("/Users/lucy/Documents/R_workfolder/Reference_areas")
# RefAreas=readOGR(".","ReferenceAreas_longlat_dissolved_updated")
# RefAreas<-spTransform(RefAreas,CRS("+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# plot(RefAreas)
# save(RefAreas)
