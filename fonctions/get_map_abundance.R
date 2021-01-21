get_map_abundance <- function(empty.map,
															dsm.pred,
															predata_tmp,
															session_selec,
															segdata,
															distdata,
															pal = c("#C2D7D9", "#CC2227"),
															abondance = TRUE,
															transects = TRUE,
															observations = TRUE,
															poster = FALSE) {
	
	# On passe en % de requins
	dsm.pred.sum <- sum(dsm.pred)
	dsm.pred <- (dsm.pred/dsm.pred.sum)*100
	
	# Carte vide
	res <- empty.map
	

	# Lignes des transects
	if (transects) {
		res <- res +
			geom_path(
				data = segdata,
				mapping = aes(x = longitude, y = latitude, group = Transect.Label),
				size = 0.3,
				colour = alpha("black", 0.2)
			)
	}
	
	# Points de la session
	if (observations) {
		res <- res +
			geom_point(data = (distdata %>% filter(session == session_selec)),
								 aes(x = longitude, y = latitude), colour = "#004547", shape = "🦈", size = .9)
	}
	
	# Raster d'abondance des requins
	if (abondance) {
		res <- res +
			geom_raster(data = as.data.frame(
				cbind(
					x = predata_tmp$longitude,
					y = predata_tmp$latitude,
					abondance = dsm.pred
				)
			),
			aes(x = x, y = y, fill = abondance,
					text = paste0(round(dsm.pred, 2), "% des requins de la zone dans cette cellule de 4km²")),
			alpha = 0.8) +
			coord_fixed(ratio = 1.5) +
			scale_fill_gradientn(
				colours = pal,
				name = ""
			)
		
		if (poster){
			res <- res +
				guides(
					col = FALSE,
					fill = guide_colourbar(
						ticks = F,
						barwidth = 4,
						barheight = 28,
						label.theme = element_text(color = "#4B5755", size = 40),
						direction = "vertical"
					)
				)}
	}
	
	
	# Thème et couleurs
	res <- res + 
		theme_void()
	

	# Si poster = FALSE, conversion ggplotly
	if (poster == FALSE){
		
		axis_layout = list(showgrid = F, showline = FALSE)
		
		res <- res + theme(legend.position = "none")
		
		res <- ggplotly(res, tooltip = "text") %>%
			layout(
				autosize = TRUE,
				xaxis = axis_layout,
				yaxis = axis_layout
			)
		
	}
	
	return(res)
}
