library(bslib)
library(leaflet)
library(leafgl)
library(data.table)
library(sf)
library(shinyWidgets)
library(highcharter)
library(shinyjs)
library(shinyscreenshot)
library(shinymanager)
# library(rmarkdown)
# library(capture)

# shinyWidgets::shinyWidgetsGallery()
# webshot::install_phantomjs()

options(shiny.fullstacktrace=TRUE)

credentials <- data.frame(
  user = c("transitar", "etufor"), # mandatory
  password = c("transitarsenha", "etuforsenha"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA, "2025-12-31"),
  admin = c(TRUE, FALSE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)