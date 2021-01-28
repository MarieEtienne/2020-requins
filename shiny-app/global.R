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
library(dsm)
library(plotly)


# Importation des données  ------------------------------------------------
load("data/detfc.Rdata")
load("data/modeles_dsm.RData")
load("data/modeles_dsm.pred.RData")
load("../../data/donnees_nettoyees.RData")
load("data/emptymap.RData")
source("fonctions/get_map_abundance_app.R")



