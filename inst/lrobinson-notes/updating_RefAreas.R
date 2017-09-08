# create revised Ross Sea Reference Area
library(maptools)
library(CCAMLRGIS)

SSRUs<- load_SSRUs("GEOJSON")

RSR_open <- SSRUs[SSRUs$GAR_Short_Label%in%c("881B","881C","881G","881H","881I","881J","881K","881L"),] 

# # Generate IDs for grouping
RSR_open_id<- rep(1,length(RSR_open$id))

RSR_open_merge=unionSpatialPolygons(RSR_open,IDs=RSR_open_id)
RSR_open<- RSR_open_merge
RSR_open$name <- "RSR_open"

# remove old Ross Sea Reference Area
RefAreas <- rbind(RefAreas,RSR_open)

# updated data in package
devtools::use_data(RefAreas, overwrite = TRUE)

# - size of data reduced, not enough

tools::resaveRdaFiles("data", compress = "auto")


