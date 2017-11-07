#Calculando o PCA com EVD - Decomposição de AutoValores, função eigen()
# Calcula valores e vectores próprios de numérico (double, inteiro, lógico) ou matrizes complexas
#A decomposição espectral de x é retornado como componentes de uma lista com os componentes
# values - um vetor que contém p autovalores de x, classificados em ordem decrescente
# vectors - Matriz qualquer p * p cujas colunas contém os autovetores de x.

pca_evd <- function(dataset, center = TRUE, scale = TRUE) {
	# Media Central e Normalização 
		X = scale(dataset, center = center, scale = scale)
	# Decomposição de AutoValores
		if (nrow(X) >= ncol(X)) {
			EVD = eigen(t(X) %*% X)
		} else {
			EVD = eigen(X %*% t(X))
		}
	# scores
		scores = X %*% EVD$vectors
		rownames(scores) = rownames(dataset)
	# loadings
		loadings = EVD$vectors
		rownames(loadings) = colnames(dataset)
	# results
		list(
			values = EVD$values / (nrow(X) - 1),
			scores = scores,
			loadings = loadings
		)
}

pca2 = pca_evd(cars2004)

# Autovalores
pca2$values

# scores
pca2$scores

# loadings
pca2$loadings

# plot of scores
plot(pca2$scores[,1], pca2$scores[,2], type = "n", xlab = "PC1", ylab = "PC2")
abline(h = 0, v = 0, col = "gray80")
text(pca2$scores[,1], pca2$scores[,2], labels = abbreviate(rownames(pca2$scores), 10), col = "#4380d377" , xpd = TRUE)

# plot of loadings
plot(pca2$loadings[,1], pca2$loadings[,2], type = "n", xlab = "axis 1", ylab = "axis 2")
abline(h = 0, v = 0, col = "gray80")
text(pca2$loadings[,1], pca2$loadings[,2], labels = rownames(pca2$loadings, 10), col = "#ff9900" , xpd = TRUE)

