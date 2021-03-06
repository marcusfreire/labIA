#PCA_with_R by Gaston Sanchez
#Usando PCA com o pacote RCurl, e conjunto de dados de carros em 2004
library(RCurl)
cars2004=read.csv("cars2004.txt", sep="\t", row.names=1, head=T)
#cars2004 = read.csv("car.csv",sep=" ", row.names=1, header = TRUE)

#Verificando o conjunto de dados
head(cars2004)

# Estat�sticas descritivas
cars_stats = data.frame(
Minimum = apply(cars2004, 2, min),
Maximum = apply(cars2004, 2, max),
Mean = apply(cars2004, 2, mean),
Std_Dev = apply(cars2004, 2, sd))

print(cars_stats, print.gap = 3)

#Uma vez que temos um pequeno n�mero de observa��es (24 carros),
#podemos usar a fun��o stars() para ter uma id�ia das (des) semelhan�as entre os carros:

stars(cars2004, labels = abbreviate(rownames(cars2004), 6),
nrow = 4, key.loc = c(8, 11.2))

abline(h = 9.85, col = "gray90")

#Para inspecionar as rela��es de pares
pairs(cars2004)

#Tamb�m podemos examinar as correla��es entre vari�veis:
as.dist(round(cor(cars2004), 3))

#A sa�da m�nima de qualquer PCA deve conter 3 coisas:
# - Autovalores fornecer informa��es sobre a quantidade de variabilidade capturado por cada componente principal
# - Scores ou PCs que fornecem coordenadas para representar graficamente objetos em um espa�o de dimens�o inferior
# - Loadings fornecer informa��es para determinar quais vari�veis caracterizam cada componente principal

#PCA com prcomp()
cars_prcomp = prcomp(cars2004, scale. = TRUE)
names(cars_prcomp)
#scale. = TRUE indica que PCA � realizada em dados padronizados (m�dia = 0, vari�ncia = 1)

# AutoValores
(cars_prcomp$sdev)^2


# scores
round(head(cars_prcomp$x, 5), 2)

# loadings
round(head(cars_prcomp$rotation, 5), 2)

#PCA com princomp()
cars_princomp = princomp(cars2004, cor = TRUE)
#cor = TRUE indica que o PCA esta com os dados padronizados(mean = 0, variance = 1)
# O que princomp() fornece?
names(cars_princomp)

# eigenvalues
(cars_princomp$sdev)^2

# scores
round(head(cars_princomp$scores, 5), 3)

# loadings
round(head(unclass(cars_princomp$loadings), 5), 3)

PCA com "FactoMineR"

# load FactoMineR
library(FactoMineR)

# nice PCA
cars_pca = PCA(cars2004, graph = FALSE)

# O que o PCA fornece?
cars_pca

#Com os escores obtidos e as cargas n�s podemos obter v�rias exibi��es gr�ficas
# alguns gr�ficos:
# - correla��es entre os escores e as vari�veis
# - rela��es entre as vari�veis
# - posi��es de objetos nas parcelas de pontua��o
# - (dis) siminalirities entre objetos
# - rela��es entre objetos e vari�veis

# Grafico dos AutoValores
barplot(cars_pca$eig[,"eigenvalue"], border = NA, col = "gray80",names.arg = rownames(cars_pca$eig))

# Correla��o entre Vari�veis e PCs
round(cars_pca$var$coord[,1:2], 4)

# Grafico Circular de Correla��es
# Quanto mais perto uma seta � a circunfer�ncia do c�rculo, maior ser� a sua representa��o nos eixos indicados.
plot(cars_pca, choix = "var")

# Contribui��o das vari�veis
print(rbind(cars_pca$var$contrib,TOTAL = colSums(cars_pca$var$contrib)), print.gap = 3)

library(RColorBrewer)
# paleta de cores
colpal = brewer.pal(n = 5, name = "Blues")[5:1]
# Contribution of variables
barplot(t(cars_pca$var$contrib), beside = TRUE,border = NA, ylim = c(0, 90), col = colpal,
legend.text = colnames(cars_pca$var$contrib),args.legend = list(x = "top", ncol = 5, bty = 'n'))

abline(h = 16, col = "#ff572255", lwd = 2)

# PC scores (2 primeiras dimens�es)
print(round(cars_pca$ind$coord[,1:2], 3), print.gap = 3)

# plot of scores
plot(cars_pca, choix = "ind")

# load ggplot2
library(ggplot2)

# data frame com as observa��es do resultado do PCA
cars_pca_obs = data.frame(cars_pca$ind$coord[,1:3])

# PCA plots of observations
ggplot(cars_pca_obs, aes(x = Dim.1, y = Dim.2, label = rownames(cars2004))) +
geom_hline(yintercept = 0, color = "gray70") +
geom_vline(xintercept = 0, color = "gray70") +
geom_point(color = "#55555544", size = 5) +
geom_text(alpha = 0.55, size = 4) +
xlab("PC1") +
ylab("PC2") +
xlim(-5, 6) +
ggtitle("PCA plot of observations")

#Contribui��o dos objetos sobre os PCs
#As contribui��es (em porcentagem) refletem a influ�ncia que cada objeto tem sobre a forma��o dos PCs.
#Se todos os objetos tinham a mesma contribui��o em cada PC, eles contribuiriam com um valor de 4,16 = 100/24

print(round(cars_pca$ind$contrib[,1:2], 3),print.gap = 3)

op = par(mfrow = c(2,1))

# barplot da contribui��o dos objetos para PC1

barplot(cars_pca$ind$contrib[,1], border = NA, las = 2,
names.arg = abbreviate(rownames(cars2004), 8), cex.names = 0.8)
title("Contribui��o dos objetos sobre PC1", cex.main = 0.9)
abline(h = 4.16, col = "gray50")

# barplot of object contributions for PC2
barplot(cars_pca$ind$contrib[,2], border = NA, las = 2,
names.arg = abbreviate(rownames(cars2004), 8), cex.names = 0.8)
title("Contribui��o dos objetos sobre PC2", cex.main = 0.9)
abline(h = 4.16, col = "gray50")
par(op)

#Agrupamento
# Como poderiam ser agrupados?
cars_clustering = hclust(dist(cars_pca$ind$coord), method = "ward")
plot(cars_clustering, xlab = "", sub = "") 

#Pc com parti��o de Clustering (Agrupamento)

# Dado 3 cluster
cars_clusters = cutree(cars_clustering, k = 3)

# add cluster to data frame of scores
cars_pca_obs$cluster = as.factor(cars_clusters)

# ggplot
ggplot(cars_pca_obs, aes(x=Dim.1, y=Dim.2, label=rownames(cars2004))) +
geom_hline(yintercept = 0, color = "gray70") +
geom_vline(xintercept = 0, color = "gray70") +
geom_point(aes(color = cluster), alpha = 0.55, size = 3) +
geom_text(aes(color = cluster), alpha = 0.55, size = 4) +
xlab("PC1") +
ylab("PC2") +
xlim(-5, 6) +
ggtitle("PCA plot of observations")
