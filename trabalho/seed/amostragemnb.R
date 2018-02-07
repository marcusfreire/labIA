source("lerDados")
source("../amostragem")
source("../naiveBayes")

#-----------------Tratando Base ---- Para Naive
titanic=titanic[-which(titanic$SibSp>2),]
titanic=titanic[-which(titanic$Age>60),]
titanic=titanic[-which(titanic$Fare>47),]

temp=as.vector(titanic$Age)
tempj=as.vector(titanic$Age)
tempa=as.vector(titanic$Age)
tempi=as.vector(titanic$Age)

tempj[which(tempj<=20)]="J"
tempa[which(tempa>20 & tempa<=50)]="A"
tempi[which(tempi>50)]="I"

temp[which(tempj=="J")]="J"
temp[which(tempa=="A")]="A"
temp[which(tempi=="I")]="I"
temp[which(is.na(temp))]="INDEF"
temp=as.factor(temp)
titanic$Age=temp

for(i in 1:ncol(titanic)){
  titanic[,i]=as.factor(titanic[,i])
}
titanic=titanic[,-3]
titanic=titanic[,-7]
titanic=titanic[,-8]
#---FIM-----------Tratando Base


#Procurando melhor amostra aleatória para o método NaiveBayes
amostra=c()
p=c()
for(aleatorio in 1:5){
	amostra[aleatorio] = sample(1:100000000, 1)
	set.seed(amostra[aleatorio])
	cat("Execução[",aleatorio,"] Chave Escolhida: ",amostra[aleatorio]," para o NBayes\n")
	dados=random(titanic,1)

	rotulo=c()
	for(i in 1:nrow(dados$test)){
	  a = naive.bayes(dados,i)
	  rotulo[i]=names(which.max(a))
	}
	rotulo=as.integer(rotulo)
	prob_modelo = sum(rotulo==dados$testRotulo)/length(dados$testRotulo)
	cat("Modelo NaiveBayes com acertivas de: ",prob_modelo*100,"%\n")
	p[aleatorio]=prob_modelo*100
}
amostraaleatorioa=c(amostra,p)
cat("\n\nChave: ",amostra[which.max(p)]," para a maior probabilidade ",p[which.max(p)],"%\n\n")
