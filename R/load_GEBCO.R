#' Load GEBCO bathymetry data
#'
#' Load GEBCO bathymetry data
#' @param data_dir is the directory into which the most current bathymetry data
#' (gebco 2014 30-arc second grid) will be downloaded and stored
#' @export
load_GEBCO <- function(data_dir){
  setwd(data_dir)
  #  use the url that stores the gebco data on our online geoserver - currently in correspondance with GEBCO/IBCSO about this
  # this works but currently bathymetry data is gebco 2008 and is just a tiff file (so just colour values no bathymetry values and no geographic information)
  # ccamlrgisgebco <- "https://gis.ccamlr.org/geoserver/gis/wms?request=GetMap&service=WMS&version=1.1.0&layers=gis:geb08south_clip40_102020&styles=&bbox=-5398150.787274787,-5398800.520136088,5398800.5201460235,5398150.787284723&width=512&height=512&srs=EPSG:102020&format=image%2Ftiff"
  ccamlrgisurl <- "https://gis.ccamlr.org/geoserver/gis/wms?version=1.1.0&request=GetMap&layers=gis:geb14_projected_cubic_resample_500_500m_depths_600_1800&styles=&bbox=-5397733.44606832,-5397766.55393168,5397766.55393168,5397733.44606832&width=512&height=512&srs=EPSG:102020&format=Gtiff"
  
  # if we upload gebco 2014 data as a GeoTiff then it should be accessible as:
  # new_test="https://gis.ccamlr.org/geoserver/gis/wms?request=GetMap&service=WMS&version=1.1.0&layers=gis:geb14south_clip40_102020&styles=&bbox=-5398150.787274787,-5398800.520136088,5398800.5201460235,5398150.787284723&width=512&height=512&srs=EPSG:102020&format=Gtiff"
  # download raster data to current file directory using the utils package
  utils::download.file(ccamlrgisurl,destfile = "bathy_data.tif")
  bath_test <- raster::raster("bathy_data.tif")
  # return bathymetry data
  bath_test
}
