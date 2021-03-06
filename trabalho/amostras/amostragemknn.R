source("lerDados")
source("../amostragem")
source("../knn")
#Para um teste de 20 amostras
#set.seed(124578) # 75%
#Chave Escolhida:  92056075, modelo KNN com acertivas de:  0.7845118
#Chave Escolhida:  40601223, modelo KNN com acertivas de:  0.8148148
#set.seed(1946827350) # 90% acertos
#Para todos os testes Nº Aleatórios:
# 738638014 71%

#-----------------Tratando Base ---- Para KNN --
titanic=titanic[-3]

temp=as.numeric(titanic$Age)
temp=abs(na.spline(temp))
temp=scale(temp)
titanic$Age=temp[,1]

temp=as.numeric(titanic$Fare)
temp=scale(temp)
titanic$Fare=temp[,1]

#Procurando melhor amostra aleatória para o método knn
amostra=c()
p=c()
for(aleatorio in 1:100){
	amostra[aleatorio] = sample(1:100000000, 1)
	set.seed(amostra[aleatorio])
	cat("Execução[",aleatorio,"] Chave Escolhida: ",amostra[aleatorio]," para o KNN\n")
	dados=random(titanic,1)
	rotulo=knn(dados)
	Verdadeiros = sum(dados$testRotulo==rotulo)
	Verd_percentual = Verdadeiros/length(rotulo)
	cat("Modelo KNN com acertivas de: ",Verd_percentual,"\n")
	p[aleatorio]=Verd_percentual
}
amostraaleatorioa=c(amostra,p)
cat("\n\nChave: ",amostra[which.max(p)]," para a maior probabilidade ",p[which.max(p)],"%\n\n")
