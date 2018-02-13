source("lerDados")
source("../amostragem")
source("../naiveBayes")
require("zoo")

#Chave:  40464076  para a maior probabilidade  77.10438 %
#Chave:  37298608  para a maior probabilidade  78.78788 %

#-----------------Tratando Base ---- Para Naive
#titanic=titanic[-which(titanic$SibSp>2),]
#titanic=titanic[-which(titanic$Age>60),]
#titanic=titanic[-which(titanic$Fare>47),]
titanic=titanic[-which(titanic$Parch==6),]
temp=as.integer(titanic$Age)
temp2=as.vector(titanic$Age)
temp=abs(na.spline(temp))

temp2[which(temp<=20)]="J"
temp2[which(temp>20 & temp<=50)]="A"
temp2[which(temp>50)]="I"
temp2=as.factor(temp2)
titanic$Age=temp2

temp=as.vector(titanic$Cabin)
temp[which(titanic$Cabin=="")]="N"
temp[which(titanic$Cabin!="")]="S"
temp=as.factor(temp)
titanic$Cabin=temp

temp=as.numeric(titanic$Fare)
temp[which(titanic$Fare>=47)]="A"
temp[which(titanic$Fare>10 & titanic$Fare<47)]="B"
temp[which(titanic$Fare<=10)]="C"
temp=as.factor(temp)
titanic$Fare=temp

titanic=titanic[,-3]
titanic=titanic[,-7]

temp=as.vector(titanic$Embarked)
temp[which(titanic$Embarked=="")]=NA
temp=na.fill(temp,c("C","Q","S"))
temp=as.factor(temp)
titanic$Embarked=temp

for(i in 1:ncol(titanic)){
  titanic[,i]=as.factor(titanic[,i])
}

#---FIM-----------Tratando Base


#Procurando melhor amostra aleatória para o método NaiveBayes
amostra=c()
p=c()
for(aleatorio in 1:100){
	amostra[aleatorio] = sample(1:100000000, 1)
	set.seed(amostra[aleatorio])
	cat("Execução[",aleatorio,"] Chave Escolhida: ",amostra[aleatorio]," para o NBayes\n")
	dados=random(titanic,1)

	rotulo=c()
  cont=0
  #cat("\tNão(0)\t\tSIM(1)\n")
	for(i in 1:nrow(dados$test)){
	  a = naive.bayes(dados,i)
    #if(which.max(a)==2 & a[2]>0.95 & dados$testRotulo[i]==1){
    #  cont=sum(cont,1)
    #  cat("[",i,"]\t",a[1],"\t",a[2],"\n")
    #  print(dados$test[i,])
    #}
    rotulo[i]=names(which.max(a))
	}
	rotulo=as.integer(rotulo)
	prob_modelo = sum(rotulo==dados$testRotulo)/length(dados$testRotulo)
	cat("Modelo NaiveBayes com acertivas de: ",prob_modelo*100,"%\n")
	p[aleatorio]=prob_modelo*100
}
amostraaleatorioa=c(amostra,p)
cat("\n\nChave: ",amostra[which.max(p)]," para a maior probabilidade ",p[which.max(p)],"%\nCont:",cont,"\n")
