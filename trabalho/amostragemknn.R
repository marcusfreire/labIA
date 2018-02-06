titanic=read.csv("dataset/train.csv",head=T)
titanic=titanic[-1]
#Para um teste de 20 amostras
#set.seed(124578) # 75%
#set.seed(1946827350) # 90% acertos
#Para todos os testes Nº Aleatórios:
# 738638014 71%

#Amostragem
houldout=function(dados,colunaRotulo){
res=c()
tamanho = nrow(dados)
p=floor(tamanho*(2/3))
cat("Tamanho :",tamanho,"p: ",p,"\n")
	rotulo=dados[,colunaRotulo]
	dados=dados[,-colunaRotulo]
	list(
		trainig=dados[1:p,],
		trainigRotulo=rotulo[1:p],
		test=dados[(p+1):tamanho,],
		testRotulo=rotulo[(p+1):tamanho]
	)
}
#houldout(titanic,1)

random=function(dados,colunaRotulo){
res=c()
tamanho = nrow(dados)
p=floor(tamanho*(2/3))
cat("Tamanho :",tamanho,"p: ",p,"\n")
	rotulo=dados[,colunaRotulo]
	dados=dados[,-colunaRotulo]
	rand=sample(1:tamanho,tamanho)
	list(
		trainig=dados[rand[1:p],],
		trainigRotulo=rotulo[rand[1:p]],
		test=dados[rand[(p+1):tamanho],],
		testRotulo=rotulo[rand[(p+1):tamanho]]
	)
}
#random(titanic,1)

#Rodando o KNN
dist=function(x,y){
	result=0
	for(j in 1:ncol(x)){
		#cat("valor de trainig ",x[,j],"Valor de Teste ",y[,j],"\n")
		if(is.numeric(x[,j])){
			#cat("Coluna ",j," eh numerico\n")
			result=result+sqrt(sum((x[,j]-y[,j])^2))
		}else{
			#cat("Coluna ",j," Não eh numerico")
			if(x[,j]==y[,j]){
				#cat(" -- Igual \n")
				#result=0
			}else{
				#cat(" -- Diferente\n")
				result=result+1
			}
		}
	}
	result
}

knn=function(dados,k=0){
	distancia=c()
	rotulo=c()
	if (!k){
		k=nrow(dados$test)
	}else{
		if (k >= nrow(dados$test)){
			stop("O valor de K é maior que o conjunto de testes")
		}
	}
	
	for(ki in 1:k){
		distancia=c()
		for(i in 1:nrow(dados$trainig)){
			distancia[i]=dist(dados$trainig[i,],dados$test[ki,])
		}

		indice=sort.list(distancia,decreasing=F)[1]
		#cat("Valor Index: ",indice," valorRotulo ",dados$trainigRotulo[indice],"\n")
		rotulo[ki]=dados$trainigRotulo[indice]
	}
	rotulo
}

#Procurando melhor amostra aleatória
a=c()
p=c()
for(aleatorio in 1:2){	
	a[aleatorio] = sample(1:1000000000, 1)
	set.seed(a[aleatorio])
	cat("Amostra Escolhida: ",a[aleatorio],"\t")
	dados=random(titanic,1)
	rotulo=knn(dados)
	Verdadeiros = sum(dados$testRotulo==rotulo)
	Verd_percentual = Verdadeiros/nrow(dados$test)
	cat("Quantidade de acertos: ",Verdadeiros,"\n")
	cat("Porcentagem de verdadeiros sobre os testes: ",Verd_percentual,"\n")
	p[aleatorio]=Verd_percentual
}
amostraaleatorioa=c(a,p)



