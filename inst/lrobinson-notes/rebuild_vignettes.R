# vignette rebuilding 

devtools::build()

rmarkdown::render("vignettes/BERT-CCAMLR.Rmd")

pkgdown::build_site()
