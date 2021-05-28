# Requirements:

# Packages:
packages <- list(
  "scales",
  "Rcpp",
  "ggplot2",
  "data.table",
  "R.utils"
)
invisible(
  lapply(
    packages,
    function(i) {
      if(!require(i, character.only = TRUE)) {
        install.packages(i)
        library(i, character.only = TRUE)
      } else {
        library(i, character.only = TRUE)
      }
    }
  )
)

# Helper functions
lapply(
  list.files("R", pattern = "\\.R"),
  function(file) {
    source(file = file.path("R",file))
  }
)
