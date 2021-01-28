selec_dsm_aic_fwd <- function(segdata,
															obsdata,
															detfc,
															vct.spline.test,
															availability,
															str.spline.selec = NULL,
															nvar = 1,
															messages = TRUE,
															n_cov_tot = NULL,
															suivi_df = NULL) {
	# Message : suivi de la progression
	if (messages) {
		print(paste0("----- Sélection de la covariable numéro ", nvar, " -----"))
	}
	
	# On récupère le nombre de covariables à tester en tout à l'itération 1
	if (nvar == 1) {
		n_cov_tot <- length(vct.spline.test)
	}
	
	# Initialisation du tableau de résultats pour chaque spline sélectionnée
	res.spl.aic <- data.frame(
		"nspline" = 1:length(vct.spline.test),
		"formule.dsm" = rep(NA, length(vct.spline.test)),
		"AIC" =  rep(NA, length(vct.spline.test))
	)
	
	# Initialisation de la liste de tous les modèles
	dsm.tmp.all <- list()
	
	# Boucle sur les covariables à tester pour récupérer l'AIC
	for (spl in 1:length(vct.spline.test)) {
		# Récupère la formule à mettre dans la fonction dsm
		if (is.null(str.spline.selec)) {
			formule.dsm <- paste0("count ~ ", vct.spline.test[spl])
		} else {
			formule.dsm <-
				paste0(str.spline.selec, " + ", vct.spline.test[spl])
		}
		
		# Faire tourner la fonction dsm
		dsm.tmp <- eval(parse(
			text = paste0(
				"dsm(
				formula = ",
				formule.dsm,
				",
				ddf.obj =  detfc,
				segment.data = segdata,
				observation.data = obsdata,
				method = 'REML',
				family = tw(),
				engine = 'gam',
				gamma = 1.4,
				availability = ",
				availability,
				")"
			)
		))
		
		# Ajout des résultats à res.spl.aic
		res.spl.aic$formule.dsm[spl] <- formule.dsm
		res.spl.aic$AIC[spl] <- dsm.tmp$aic
		eval(parse(text = paste0("dsm.tmp.all$iter", spl, " <- dsm.tmp")))
	}
	
	# Récupération des infos du modèle pour lequel l'AIC est minimal
	nspline.selec <-
		res.spl.aic$nspline[res.spl.aic$AIC == min(res.spl.aic$AIC)]
	str.spline.selec <- res.spl.aic$formule.dsm[nspline.selec]
	str.spline.AIC <- res.spl.aic$AIC[nspline.selec]
	dsm.selec <-
		eval(parse(text = paste0("dsm.tmp.all$iter", nspline.selec)))
	
	# On créé ou complète le tableau suivi_df
	if (nvar == 1) {
		suivi_df = data.frame(
			"iter" = 1,
			"formule" = str.spline.selec,
			"aic" = str.spline.AIC,
			stringsAsFactors = FALSE
		)
	} else {
		suivi_df[nvar, 1] = nvar
		suivi_df[nvar, 2] = str.spline.selec
		suivi_df[nvar, 3] = str.spline.AIC
	}
	
	# Suppression de la variable sélectionnée de vct.spline.test pour réitérer la fonction
	vct.spline.test <- vct.spline.test[-nspline.selec]
	
	res_final <- list(
		dsm.selec = dsm.selec,
		aic = dsm.selec$aic,
		formule.dsm = formule.dsm,
		suivi = suivi_df
	)
	
	# Si le nombre de coefficients estimés est inférieur au nombre de lignes de obsdata, on continue
	if (length(dsm.selec$coefficients) < nrow(obsdata) &
			nvar <= n_cov_tot) {
		# Si c'est la 1ère itération, on choisit ce modèle et on continue
		if (nvar == 1) {
			res_tmp <- selec_dsm_aic_fwd(
				segdata,
				obsdata,
				detfc,
				vct.spline.test = vct.spline.test,
				str.spline.selec = str.spline.selec,
				availability = availability,
				nvar = nvar + 1,
				messages = messages,
				n_cov_tot = n_cov_tot,
				suivi_df = suivi_df
			)
			if (res_tmp$aic < res_final$aic) {
				res_final <- res_tmp
			}
			
			# Si l'AIC diminue, on choisit ce modèle et on continue
		} else if (nvar > 1 &
							 (res_final$suivi$aic[nvar] < res_final$suivi$aic[nvar - 1])) {
			res_tmp <- selec_dsm_aic_fwd(
				segdata,
				obsdata,
				detfc,
				vct.spline.test = vct.spline.test,
				str.spline.selec = str.spline.selec,
				availability = availability,
				nvar = nvar + 1,
				messages = messages,
				n_cov_tot = n_cov_tot,
				suivi_df = suivi_df
			)
			if (res_tmp$aic < res_final$aic) {
				res_final <- res_tmp
			}
			
			# Sinon, on arrête
		} else {
			print(paste0(
				"L'AIC ne diminue plus en ajoutant une ",
				nvar,
				"ème covariable"
			))
			return(res_final)
		}
	}
	
	return(res_final)
}