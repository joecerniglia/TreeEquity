#init.R

my_packages = c("dplyr", "tidyr","terra")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages())) {
     print('Do nothing.')
  }
  else {install.packages(p,dependencies=TRUE)}
}

invisible(sapply(my_packages, install_if_missing))
