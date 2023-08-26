#init.R

my_packages = c("sf", "sfdep", "spdep", "dplyr", "tidyr", "ggplot2", "rgdal", "terra")

install_if_missing = function(p) {
  if (p %notin% rownames(installed.packages())) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
