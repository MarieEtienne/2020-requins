###############################################################################
# UI Access for Dashboard
#
# Léa Pautrel - Rindra Ranaivomanana - Emma Rouault
# Janvier 2021
###############################################################################

ui = shiny::htmlTemplate(
  # Index Page
  "www/index.html",
  
  ## Density model #############################################################
  session_choix = radioButtons(
    inputId = "session_choix",
    label = "Session",
    choices = c("Spring" = "2",
                "Summer" = "3")),
  
  availability_choix = radioButtons(
    inputId = "availability_choix",
    label = "Availability bias",
    choices = c("1" = "1",
                "0.41" = "041",
                "on-shelf/off-shelf" = "shelf")),
  
  img_splines_dsm = imageOutput("img_splines_dsm"),
  
  ## Résultats #################################################################
  
  ############HISTOGRAMME##############
  histogramme = plotlyOutput("histogramme"),
  
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
  show_transect2 = checkboxInput("transect2", 
              label = "Show transects",
              value = FALSE),
  
  show_transect3 = checkboxInput("transect3", 
              label = "Show transects",
              value = FALSE),
  
  ##Choix Observation##
  show_observations2 = checkboxInput("observations2", 
              label = "Show observations",
              value = FALSE),
  
  show_observations3 = checkboxInput("observations3", 
              label = "Show observations",
              value = FALSE),
  
  ##Cartes##
  
  session2 = plotlyOutput("session2"),

  session3 = plotlyOutput("session3")
  
  )
  
  