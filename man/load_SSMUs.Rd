% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.R
\name{load_SSMUs}
\alias{load_SSMUs}
\title{Load CCAMLR Small Scale Management Units}
\usage{
load_SSMUs(format)
}
\arguments{
\item{format}{"GEOJSON" will extract this geographical reference data displayed on the CCAMLR GIS website and "RDATA" will use the Spatial Polygon Data Frame last saved with the package}
}
\description{
Load CCAMLR Small Scale Management Units (SSMUs)
}
\examples{
 
# if online
\dontrun{
SSMUs <- load_SSMUs("GEOJSON")
}

# if offline 
SSMUs <- load_SSMUs("RDATA")
}
\keyword{(SSMUs)}
\keyword{Management}
\keyword{Scale}
\keyword{Small}
\keyword{Units}
