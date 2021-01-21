###############################################################################
# UI Access for Dashboard
#
# Léa Pautrel - Rindra Ranaivomanana - Emma Rouault
# Janvier 2021
###############################################################################

ui = shiny::htmlTemplate(
  # Index Page
  "www/index.html",
  
  
  ###########SESSIONS#################
  #SESSION 2 AV1
  num_sharks_s2_av1 = textOutput(
    "num_sharks_s2_av1",
    inline = T
  ),
  
  #SESSION 2 AV041
  num_sharks_s2_av041 = textOutput(
    "num_sharks_s2_av041",
    inline = T
  ),

  #SESSION 2 AVSHELF
  num_sharks_s2_avshelf = textOutput(
    "num_sharks_s2_avshelf",
    inline = T
  ),
  
  #SESSION 3 AV1
  num_sharks_s3_av1 = textOutput(
    "num_sharks_s3_av1",
    inline = T
  ),
  
  #SESSION 3 AV041
  num_sharks_s3_av041 = textOutput(
    "num_sharks_s3_av041",
    inline = T
  ),
  
  #SESSION 3 AVSHELF
  num_sharks_s3_avshelf = textOutput(
    "num_sharks_s3_avshelf",
    inline = T
  ),
  
  ###########SESSIONS# SE################
  #SESSION 2 AV1
  num_sharks_s2_av1_se = textOutput(
    "num_sharks_s2_av1_se",
    inline = T
  ),
  
  #SESSION 2 AV041
  num_sharks_s2_av041_se = textOutput(
    "num_sharks_s2_av041_se",
    inline = T
  ),
  
  #SESSION 2 AVSHELF
  num_sharks_s2_avshelf_se = textOutput(
    "num_sharks_s2_avshelf_se",
    inline = T
  ),
  
  #SESSION 3 AV1
  num_sharks_s3_av1_se = textOutput(
    "num_sharks_s3_av1_se",
    inline = T
  ),
  
  #SESSION 3 AV041
  num_sharks_s3_av041_se = textOutput(
    "num_sharks_s3_av041_se",
    inline = T
  ),
  
  #SESSION 3 AVSHELF
  num_sharks_s3_avshelf_se = textOutput(
    "num_sharks_s3_avshelf_se",
    inline = T
  ),
  
  
  # NOS CARTES##############################################################
  
  ##Choix transect##
  show_transect2 = radioButtons("transect2", 
              "Show transects",
              choices = list("Yes" = "TRUE", "No" = "FALSE")),
  
  show_transect3 = radioButtons("transect3", 
              "Show transects",
              choices = list("Yes" = "TRUE", "No" = "FALSE")),
  
  ##Choix Observation##
  show_observations2 = radioButtons("observations2", 
              "Show observations",
              choices = list("Yes" = "TRUE", "No" = "FALSE")),
  
  show_observations3 = radioButtons("observations3", 
              "Show observations",
              choices = list("Yes" = "TRUE", "No" = "FALSE")),
  
  ##Cartes##
  
  session2 = plotlyOutput("session2"),

  session3 = plotlyOutput("session3")
  
  )
  
  