selec_detfc_aic <- function(distancedata, list.cov, list.key) {
	# On récupère le nombre de combinaisons possibles
	## 2^(nombre de covariables) : nombre de combinaisons possibles pour les covariables
	## * nb de key : 1 seule key à chaque fois
	n_comb <- 2 ^ (length(list.cov)) * length(list.key)
	
	# On récupère les combinaisons pour list.cov
	comb_list.cov <-
		do.call(data.table::CJ, replicate(length(list.cov), 0:1, FALSE))
	names(comb_list.cov) <- list.cov
	comb_list.cov[rep(1:nrow(comb_list.cov), length(list.key)), 1:length(list.cov)]
	
	# Initialisation : tableau des résultats vide
	res <- as.data.frame(cbind(
		comb_list.cov[rep(1:nrow(comb_list.cov), length(list.key)), 1:length(list.cov)],
		data.frame(
			"formula" = rep(NA, n_comb),
			"key" = rep(list.key, each = 2 ^ (length(list.cov))),
			"AIC" =  rep(NA, n_comb)
		)
	))
	
	detfc_res <- list()
	
	# On lance une double boucle qui va calculer pour chaque possibilité l'AIC
	i = 0
	for (k in 1:length(list.key)) {
		for (c in 1:2 ^ (length(list.cov))) {
			i = i + 1 # Numéro de ligne pour res
			
			
			# key ----
			#	res$key[i] = list.key[k] # Insertion de key dans res pour la ligne i
			
			# formula ----
			formula <- "~"
			for (col in 1:length(list.cov)) {
				if (res[i, col] == 1) {
					formula <- paste(formula, names(res)[col], "+")
				}
			}
			if (formula == "~") {
				formula = "~1"
			} else {
				formula = substr(formula, 1, nchar(formula) - 2)
				formula = gsub(" ", "", formula)
			}
			res$formula[i] = formula # Insertion de formula dans res pour la ligne i
			
			# AIC ----
			erreur <- try(tmp <- eval(parse(
				text = paste(
					"Distance::ds(
					distancedata,
					max(distancedata$distance),
					formula = ", formula, ",
					key = '", list.key[k], "'
				)", sep = ""
				)
			)),
			silent = T)
			if (is(erreur, "try-error")) {
				print("Erreur Distance::ds")
				print(erreur)
				out_AIC <- NA
			} else {
				tmp <- eval(parse(
					text = paste(
						"Distance::ds(
						distancedata,
						max(distancedata$distance),
						formula = ", formula, ",
						key = '", list.key[k], "'
					)", sep = ""
					)
				))
				eval(parse(text = paste("detfc_res$detfc$iter", i, " <- tmp", sep = "")))
				out_AIC <- AIC(tmp)$AIC
			}
			res$AIC[i] = out_AIC # Insertion de AIC dans res pour la ligne i
			
			# Affichage
			print(paste("Itération", i, "sur", n_comb, "terminée."))
		}
	}
	
	return(list(res, detfc_res))
}