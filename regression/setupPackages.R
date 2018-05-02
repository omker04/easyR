packages_required <- c("dtplyr","data.table", "ggplot2", "shiny", "shinydashboard", "shinyjs", "shinyBS", "DT", 'miniUI')

packages_installed <- row.names(installed.packages()) # already installed packages
packages_to_install <- setdiff(packages_required, packages_installed) # installation required

lapply(packages_to_install, function(pkg){install.packages(pkg, dependencies = TRUE)}) # installing
lapply(packages_required, function(pkg){library(pkg, character.only = TRUE)}) # loading in directory

rm(packages_required, packages_installed, packages_to_install)