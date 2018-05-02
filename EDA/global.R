reqdPkgs <- c('rstudioapi', 'miniUI', 'shiny', 'shinydashboard', 'shinyjs', 'shinyBS', 'devtools')
installedPkgs <- installed.packages()

pkgsToBeInstalled <- setdiff(reqdPkgs, installedPkgs)

lapply(pkgsToBeInstalled, function(x) install.packages(x, dependencies = TRUE))
lapply(reqdPkgs, function(x) library(x, character.only = TRUE))

stableColumnLayout <- function(...) {
  dots <- list(...)
  n <- length(dots)
  width <- 12 / n
  class <- sprintf("col-xs-%s col-md-%s", width, width)
  fluidRow(
    lapply(dots, function(el) {
      div(class = class, el)
    })
  )
}

isErrorMessage <- function(object) {
  inherits(object, "error_message")
}

errorMessage <- function(type, message) {
  structure(
    list(type = type, message = message),
    class = "error_message"
  )
}