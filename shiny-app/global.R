###############################################################################
# Entrypoint for the shiny app
#
# Author: Léa Pautrel, Rindra Ranaivomanana, Emma Rouault
# Created 2019-01-30 19:34:54
###############################################################################


# Dependencies ------------------------------------------------------------
library(shiny)
library(tidyverse)
library(janitor)
library(lubridate)
library(leaflet)
library(shinycssloaders)
library(sp)

#  Clean Scripts ----------------------------------------------------------
source("utils/clean_uber_data.R")

# Importation des données  ------------------------------------------------
load("../resultats/detfc.Rdata")
load("../resultats/modeles_dsm.RData")
load("../resultats/modeles_dsm.pred.RData")
