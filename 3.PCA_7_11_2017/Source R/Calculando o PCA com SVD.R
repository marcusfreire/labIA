#Calculando o PCA com SVD
# escala é função genérica cujo padrão método centros e / ou escala as colunas de uma matriz numérica.
#scale()
# Valor singular de decomposição (SVD), utilizando a função svd()
# A função SVD(X, nu,nv),nu e nv são calculados automaticamente, são os minimos da direita e esquerda da matriz
# 	nu - o número de vetores esquerdos singulares para ser computada. Estes dados devem entre 0 e n = nrow (x).
# 	nv - o número de vetores direitos singulares para ser computada. Estes dados devem estar entre 0 e p = Ncol (x).
# Função SVD retorna:
# 	d - um vector que contém os valores singulares de X, de comprimento min (n, p).
#	u - a matriz cujas colunas contém os vetores esquerdo singulares de x, se presente nu> 0. Dimensão c (n, nu).
# 	v - a matriz cujas colunas contém os vetores direito singulares de x, presente se nv> 0. Dimensão c (p, nv).
#Função PCA com o SVD

pca_svd <- function(dataset, center = TRUE, scale = TRUE) {
	# Media central e normalização
		X = scale(dataset, center = center, scale = scale)
	# Valor singular de decomposição
		SVD = svd(X)
	# scores
		scores = SVD$u %*% diag(SVD$d)
		rownames(scores) = rownames(dataset)
	# loadings
		loadings = SVD$v
		rownames(loadings) = colnames(dataset)
	# Resultado - Multilicação de lagrangian
		list(
			values = SVD$d^2 / (nrow(X) - 1),
			scores = scores,
			loadings = loadings
		)
}


pca1 = pca_svd(cars2004)

# AutoValores
pca1$values

head(pca1$scores, n = 5)

# plot of scores
plot(pca1$scores[,1], pca1$scores[,2], type = "n", xlab = "PC1", ylab = "PC2", xlim = c(-6, 6), ylim = c(-3, 3))
abline(h = 0, v = 0, col = "gray80")
text(pca1$scores[,1], pca1$scores[,2], labels = abbreviate(rownames(pca1$scores), 10), col = "#4380d377" , xpd = TRUE)

# plot of loadings
plot(pca1$loadings[,1], pca1$loadings[,2], type = "n", xlab = "axis 1", ylab = "axis 2")
abline(h = 0, v = 0, col = "gray80")
text(pca1$loadings[,1], pca1$loadings[,2], labels = rownames(pca1$loadings, 10), col = "#ff9900" , xpd = TRUE)
