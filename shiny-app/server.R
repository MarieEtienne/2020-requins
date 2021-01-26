###############################################################################
# Defining Server Logic behind App to explore Shark
#
# Léa Pautrel - Rindra Ranaivomanana - Emma Rouault
# Janvier 2021
###############################################################################

server <- function(input, output) {
  
  # Basic Numbers Page --------------------------------------------------------------
  
  # Nombres de requins dans session 2
  output$num_sharks_s2_av1 <- reactive({
    dsm_s2_av1.pred$fit %>% sum() %>% round()
  })
  
  output$num_sharks_s2_av041 <- reactive({
    dsm_s2_av041.pred$fit %>% sum() %>% round()
  })
  
  output$num_sharks_s2_avshelf <- reactive({
    dsm_s2_avshelf.pred$fit %>% sum() %>% round()
  })
  
  # Nombres de requins dans session 3
  output$num_sharks_s3_av1 <- reactive({
    dsm_s3_av1.pred$fit %>% sum() %>% round()
  })
  
  output$num_sharks_s3_av041 <- reactive({
    dsm_s3_av041.pred$fit %>% sum() %>% round()
  })
  
  output$num_sharks_s3_avshelf <- reactive({
    dsm_s3_avshelf.pred$fit %>% sum() %>% round()
  })

  # Nombres de requins (se) dans session 2
  output$num_sharks_s2_av1_se <- reactive({
    dsm_s2_av1.pred$se.fit %>% sum() %>% round()
  })
  
  output$num_sharks_s2_av041_se <- reactive({
    dsm_s2_av041.pred$se.fit %>% sum() %>% round()
  })
  
  output$num_sharks_s2_avshelf_se <- reactive({
    dsm_s2_avshelf.pred$se.fit %>% sum() %>% round()
  })
  
  # Nombres de requins (se) dans session 3
  output$num_sharks_s3_av1_se <- reactive({
    dsm_s3_av1.pred$se.fit %>% sum() %>% round()
  })
  
  output$num_sharks_s3_av041_se <- reactive({
    dsm_s3_av041.pred$se.fit %>% sum() %>% round()
  })
  
  output$num_sharks_s3_avshelf_se <- reactive({
    dsm_s3_avshelf.pred$se.fit %>% sum() %>% round()
  })
  


  # Notre carte ----------------------------------------------------
  
  output$session2 <- renderPlotly({
    session2 <- get_map_abundance(empty.map,
                      dsm.pred = dsm_s2_av1.pred$fit,
                      predata_tmp = predata_tmp2,
                      session_selec = 2,
                      segdata,
                      distdata,
                      pal = c("#C2D7D9", "#CC2227"),
                      abondance = TRUE,
                      transects = input$transect2,
                      observations = input$observations2,
                      poster = FALSE)
  })
  
  output$session3 <- renderPlotly({
    session3 <- get_map_abundance(empty.map,
                                  dsm.pred = dsm_s3_av1.pred$fit,
                                  predata_tmp = predata_tmp3,
                                  session_selec = 3,
                                  segdata,
                                  distdata,
                                  pal = c("#C2D7D9", "#CC2227"),
                                  abondance = TRUE,
                                  transects = input$transect3,
                                  observations = input$observations3,
                                  poster = FALSE)
  })
  

}
