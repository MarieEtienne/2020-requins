###############################################################################
# Entrypoint for the shiny app
#
# Léa Pautrel - Rindra Ranaivomanana - Emma Rouault
# Janvier 2021
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
load("../../data/donnees_nettoyees.RData")
load("../resultats/emptymap.RData")
