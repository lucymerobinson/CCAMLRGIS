% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CCAMLRGIS.R
\docType{package}
\name{CCAMLRGIS}
\alias{CCAMLRGIS}
\alias{CCAMLRGIS-package}
\title{CCAMLRGIS: An R package for loading and creating geographic data that is compatible with the online CCAMLR GIS}
\description{
The CCAMLRGIS package provides two categories of functions: load functions and create functions
}
\section{load functions are used to import CCAMLR Reference into R (ideally from the geoserver) and include}{

load_ASDs, load_SSRUs, load_RBs, load_SSMUs, load_MAs, load_Coastline, load_RefAreas, load_MPAs, load_EEZs provide easy loading of CCAMLR Reference data 
Data that is classified as "master" data is available in "GEOJSON" format and "RDATA" format is available in all load functions
}

\section{create functions are used to create spatial data that is compatible with CCAMLR Reference data and the CCAMLR online GIS. The include}{

 create_Polys and create_PolyGrids
 Other functions that support the create functions include Clip2Coast and DensifyData
}

\section{Vignettes}{

To learn more about CCAMLRGIS, start with the vignettes:
\code{browseVignettes(package = "CCAMLRGIS")}
}

