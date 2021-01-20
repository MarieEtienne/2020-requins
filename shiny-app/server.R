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

  
  


  # Notre carte ----------------------------------------------------
  
  output$session2 <- renderPlotly({
    session2 <- get_map_abundance(empty.map,
                      dsm.pred = dsm_s2_av1.pred,
                      predata_tmp = predata_tmp2,
                      session_selec = 2,
                      segdata,
                      distdata,
                      pal = c("#C2D7D9", "#CC2227"),
                      abondance = TRUE,
                      transects = TRUE,
                      observations = TRUE,
                      poster = FALSE)
  })
  
  output$session3 <- renderPlotly({
    session3 <- get_map_abundance(empty.map,
                                  dsm.pred = dsm_s3_av1.pred,
                                  predata_tmp = predata_tmp3,
                                  session_selec = 3,
                                  segdata,
                                  distdata,
                                  pal = c("#C2D7D9", "#CC2227"),
                                  abondance = TRUE,
                                  transects = TRUE,
                                  observations = TRUE,
                                  poster = FALSE)
  })
  

}
