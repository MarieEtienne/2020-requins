###############################################################################
# UI Access for Dashboard
#
# Léa Pautrel - Rindra Ranaivomanana - Emma Rouault
# Janvier 2021
###############################################################################

ui = shiny::htmlTemplate(
  # Index Page
  "www/index.html",
  
  # Nombre de session
  number_of_trips = "4",
  
  
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
  
  # City Selector
  city_selector = selectInput(
    "city", 
    label = "Select City", 
    choices = d_clean$city %>% 
      unique(),
    selected = "Auckland"
    ),
  
  
  # Selector for Time
  time_selector = sliderInput(
    "time", 
    "Date",
    min(d_routes$request_time) %>% as.Date(), 
    max(d_routes$request_time) %>% as.Date(),
    value = max(d_routes$request_time) %>% as.Date(),
    step = 30,
    animate = animationOptions(
      playButton = HTML("<img src='images/icons/play-button.png' height='42' width='42'>"), 
      pauseButton = HTML("<img src='images/icons/pause-button.png' height='42' width='42'>")
      )
  ),
  
  # Leaflet map
  leaflet_map = leafletOutput(outputId = "map") %>% 
    withSpinner(color="#0dc5c1")
  )
