get_map_abundance <- function(empty.map,
															dsm.pred,
															predata_tmp,
															session_selec,
															segdata,
															distdata,
															pal){
	# Carte vide
	res <- empty.map +

		# Raster d'abondance des requins
		geom_raster(data = as.data.frame(
			cbind(
				x = predata_tmp$longitude,
				y = predata_tmp$latitude,
				abondance = dsm.pred
			)
		),
		aes(x = x, y = y, fill = abondance)) +

		# Lignes des transects
		geom_path(
			data = segdata,
			mapping = aes(x = longitude, y = latitude, group = Transect.Label),
			size = 0.3,
			colour = alpha("black", 0.2)
		) +

		# Points de la session
		geom_point(data = (distdata %>% filter(session == session_selec)),
							 aes(x = longitude, y = latitude, col = session)) +

		# Thème et couleurs
		theme_void() +
		# scale_fill_viridis() +
		scale_fill_gradientn(colours = pal, name = " ") +
		scale_color_manual(values = wesanderson::wes_palette("BottleRocket1", n = 4)) +
		coord_fixed(ratio = 1.5) +
		guides(col = FALSE,
					 fill = guide_colourbar(barwidth = 0.5, barheight = 10))
	
	return(res)
	
}