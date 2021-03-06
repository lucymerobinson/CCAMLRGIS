% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.R
\name{load_SSRUs}
\alias{load_SSRUs}
\title{load CCAMLR SSRUs}
\usage{
load_SSRUs(format)
}
\arguments{
\item{format}{"GEOJSON" will extract this geographical reference data displayed on the CCAMLR GIS website and "RDATA" will use the Spatial Polygon Data Frame last saved with the package}
}
\description{
Load CCAMLR Small Scale Research Units (SSRUs)
}
\examples{
 
# if online
\dontrun{
SSRUs <- load_SSRUs("GEOJSON")
}
# if offline 
SSRUs <- load_SSRUs("RDATA")
}
\keyword{Research}
\keyword{SSRU}
\keyword{Scale}
\keyword{Small}
\keyword{Units}
