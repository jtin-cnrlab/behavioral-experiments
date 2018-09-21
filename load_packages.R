# load_packages.R
# Jessica Tin
# 19 Sept 2018
#
# Defines load_packages() for loading multiple packages succinctly.
#
# load_packages()
#     args: Package names, passed as strings.
#  returns: Loads input packages, installing them first if missing.
#      ex.: load_packages("dplyr", "ggplot2")

load_packages <- function(pkg1, pkg2, ...) {
    invisible(lapply(c(pkg1, pkg2, ...), function(x) {
        # load the package if it's installed; else, install and load it
        if(!require(x, character.only = TRUE)) {
            install.packages(x)
            library(x, character.only = TRUE)
        }
    })
    )
}