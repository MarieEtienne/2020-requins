###############################################################################
# Defining Server Logic behind App to explore UBER data
#
# Léa Pautrel - Rindra Ranaivomanana - Emma Rouault
# Janvier 2021
###############################################################################

server <- function(input, output) {
  
  # Basic Numbers Page --------------------------------------------------------------
  
  # Nombres de requins dans session 2
  output$num_sharks_s2_av1 <- reactive({
    dsm_s2_av1.pred %>% sum() %>% round()
  })
  
  output$num_sharks_s2_av041 <- reactive({
    dsm_s2_av041.pred %>% sum() %>% round()
  })
  
  output$num_sharks_s2_avshelf <- reactive({
    dsm_s2_avshelf.pred %>% sum() %>% round()
  })
  
  # Nombres de requins dans session 3
  output$num_sharks_s3_av1 <- reactive({
    dsm_s3_av1.pred %>% sum() %>% round()
  })
  
  output$num_sharks_s3_av041 <- reactive({
    dsm_s3_av041.pred %>% sum() %>% round()
  })
  
  output$num_sharks_s3_avshelf <- reactive({
    dsm_s3_avshelf.pred %>% sum() %>% round()
  })

  
  


  
  
  


  # Create Map Plot ---------------------------------------------------------

  
  points_full <- reactive({
    # Clean trip data
    d_show <- d_routes %>% 
      filter(city == input$city)
    
    # Check if any trips present otherwise return NULL
    if (nrow(d_show) > 0) {
      
      # store in DF
      d_show <- d_show %>% unnest(route)
      
      # Convert to SP obj
      split_data = lapply(
        unique(d_show$trip), 
        function(x) {
          df = as.matrix(d_show[d_show$trip == x, c("lon", "lat")])
          lns = Lines(Line(df), ID = x)
          return(lns)
        }
      )
      
      # Convert to SP lines so it can be plotted
      data_lines = SpatialLines(split_data)
      
    } else {
      NULL
    }
    
  })
  
  points <- reactive({
    
    # Clean trip data
    d_show <- d_routes %>% 
      filter(city == input$city) %>% 
      filter(request_time <= (input$time))
    
    # Check if any trips present otherwise return NULL
    if (nrow(d_show) > 0) {
      
      # store in DF
      d_show <- d_show %>% unnest(route)
      
      # Convert to SP obj
      split_data = lapply(
        unique(d_show$trip), 
        function(x) {
          df = as.matrix(d_show[d_show$trip == x, c("lon", "lat")])
          lns = Lines(Line(df), ID = x)
          return(lns)
        }
      )
      
      # Convert to SP lines so it can be plotted
      data_lines = SpatialLines(split_data)
      
    } else {
      NULL
    }
    
  })
  
  output$map <- renderLeaflet({
    # Base map
    leaflet(points_full()) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels)
    
  })
  
  observe({
    req(!is.null(points()))
    # create the map
    leafletProxy("map", data = points()) %>% 
      clearShapes() %>% 
      addPolylines(weight = 1, color = "violet") %>% 
      fitBounds(
        points_full()@bbox[1], 
        points_full()@bbox[2], 
        points_full()@bbox[3], 
        points_full()@bbox[4]
      )
  })
  

}
