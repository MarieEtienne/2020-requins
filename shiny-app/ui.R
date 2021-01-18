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
  
  
  # Leaflet map
  plotly_map = plotlyOutput(outputId = "map") %>% 
    withSpinner(color="#0dc5c1")
  )
