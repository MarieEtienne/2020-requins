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
library(plotly)
library(shinycssloaders)
library(sp)
# Packages
library(dplyr)				# tidyverse
library(foreign)			# read.dbf
library(lubridate)		# dates

# Graphes packages
library(ggplot2) ; ggplot2::theme_set(theme_light())
library(ggmap)
library(viridis)
library(ggpubr)
library(plotly)

# Packages calcul
library(Distance)
library(dsm)

# Packages raster/carto
library(sp)
library(rgdal)
library(raster)

#  Clean Scripts ----------------------------------------------------------
source("utils/clean_uber_data.R")

# Importation des données  ------------------------------------------------
load("../resultats/detfc.Rdata")
load("../resultats/modeles_dsm.RData")
load("../resultats/modeles_dsm.pred.RData")
load("../../data/donnees_nettoyees.RData")
load("../resultats/emptymap.RData")
