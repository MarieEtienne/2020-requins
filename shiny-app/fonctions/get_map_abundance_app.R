get_map_abundance_app <- function(empty.map,
															dsm.pred.1, dsm.pred.041, dsm.pred.shelf,
															predata_tmp,
															session_selec,
															segdata,
															distdata,
															pal = c("#C2D7D9", "#CC2227"),
															abondance = TRUE,
															transects = TRUE,
															observations = TRUE) {
	
  # On récupère les valeurs des dsm.pred
  dsm.pred.all <- list("av1" = dsm.pred.1$fit, 
                       "av041" = dsm.pred.041$fit, 
                       "avshelf" = dsm.pred.shelf$fit)
  dsm.pred.all.percent <- dsm.pred.all
  
	# On passe en % de requins
  for (i in 1:3){
    dsm.pred.all.percent[[i]] <- (dsm.pred.all[[i]]/sum(dsm.pred.all[[i]]))*100
  }
  
  # On calcule pour chaque cellule la densité (requins/km²) selon le biais de dispo
  # N estimés total * % estimés cellule / 100 * area
  dsm.pred.all.parkm2 <- list(
    "av1" = (( sum(dsm.pred.all[[1]]) * dsm.pred.all.percent[[1]] ) / (100 * predata_tmp$Area )),
    "av041" = (( sum(dsm.pred.all[[2]]) * dsm.pred.all.percent[[2]] ) / (100 * predata_tmp$Area )),
    "avshelf" = (( sum(dsm.pred.all[[3]]) * dsm.pred.all.percent[[3]] ) / (100 * predata_tmp$Area ))
  )
	
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
			  # Ajout du raster 1
				geom_raster(
					data = as.data.frame(
						cbind(
							x = predata_tmp$longitude,
							y = predata_tmp$latitude,
							abondance = dsm.pred.all.percent[[1]]
						)
					),
					aes(
						x = x,
						y = y,
						fill = abondance,
						text = paste0(
							round(dsm.pred.all.percent[[1]], 2),
							"% of the sharks in the study area are in this 4km\u00B2 cell\n",
							"     Availability 1 : ", round(dsm.pred.all.parkm2[[1]], 2), " sharks/km\u00B2\n",
							"     Availability 0.41 : ", round(dsm.pred.all.parkm2[[2]], 2), " sharks/km\u00B2\n",
							"     Availability on/off-shelf : ", round(dsm.pred.all.parkm2[[3]], 2), " sharks/km\u00B2"
						)
					),
					alpha = 0.8
				) +
			  # Changement coordonnées
				coord_fixed(ratio = 1.5) +
			  # Changement des couleurs
			  scale_fill_gradientn(
			    colours = pal,
			    name = "",
			    limits = c(0, 0.35),
			    breaks = seq(
			      from = 0,
			      to =  0.35,
			      length.out = 6)
			  )
	}
	
	
	# Thème et couleurs
	res <- res + 
		theme_void()
	

	# Conversion ggplotly
	axis_layout = list(showgrid = F, showline = FALSE)
	
	res <- res + theme(legend.position = "none")
	
	res <- ggplotly(res, tooltip = "text") %>%
	  layout(autosize = TRUE,
	         xaxis = axis_layout,
	         yaxis = axis_layout)
	
	
	
	return(res)
}
