devtools::document(roclets = c("rd", "collate", "namespace"))
devtools::check(cleanup = FALSE, args = c("--as-cran"))
devtools::build_vignettes()
pkgdown::build_site()
