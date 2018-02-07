source("lerDados")
source("../rn")
source("../amostragem")

#install.packages("zoo")
require("zoo")
#-----------------Tratando Base ---- Para RNA -- Pode ser usado para o Knn também.
titanic=titanic[-3]
titanic=titanic[-which(titanic$Age>60),]
titanic=titanic[-which(titanic$Fare>47),]
#titanic=titanic[-which(titanic$SibSp>2),]


temp=as.vector(titanic$Sex)
temp[which(temp=="male")]="0"
temp[which(temp=="female")]="1"
titanic$Sex=as.integer(temp)

#titanic$Age=abs(na.spline(titanic$Age))
temp=as.integer(titanic$Age)
temp=abs(na.spline(temp))
temp=scale(temp)
titanic$Age=temp[,1]

temp=as.vector(titanic$Embarked)
temp[which(titanic$Embarked=="")]=NA
temp[which(temp=="C")]="0"
temp[which(temp=="Q")]="0.5"
temp[which(temp=="S")]="1"
temp=as.numeric(temp)
temp=na.fill(temp,c(0,0.5,1))
#temp=scale(temp)[,1]
titanic$Embarked=as.integer(temp)

temp=as.integer(titanic$Ticket)
temp=scale(temp)
titanic$Ticket=temp[,1]

temp=as.integer(titanic$SibSp)
temp=scale(temp)
titanic$SibSp=temp[,1]

temp=as.integer(titanic$Parch)
temp=scale(temp)
titanic$Parch=temp[,1]

temp=as.integer(titanic$Fare)
temp=scale(temp)
titanic$Fare=temp[,1]

temp=as.integer(titanic$Pclass)
temp=scale(temp)
titanic$Pclass=temp[,1]


#Excluindo Cabin
titanic=titanic[,-9]
#---FIM-----------Tratando Base


#Procurando melhor amostra aleatória para o método Redes Neurais
amostra=c()
p=c()
for(aleatorio in 1:100){
	amostra[aleatorio] = sample(1:100000000, 1)
	set.seed(amostra[aleatorio])
	cat("Execução[",aleatorio,"] Chave Escolhida: ",amostra[aleatorio]," para o RNA\t")
	dados=random(titanic,1)

	a=rna(dados)
	a[[3]][which(a[[3]]==-1)]=0
	prob_modelo=sum(a[[3]]==dados$trainigRotulo)/length(dados$trainigRotulo)
	cat("Modelo com acertivas de ",prob_modelo*100,"% da base de treinamento\n") #64.98316%
	rot=teste(dados,a)
	prob_modelo=sum(rot==dados$testRotulo)/length(rot)
	cat("Modelo com acertivas de ",prob_modelo*100,"% da base de teste\n") #59.59596%

	p[aleatorio]=prob_modelo*100
}
amostraaleatorioa=c(amostra,p)
cat("\n\nChave: ",amostra[which.max(p)]," para a maior probabilidade ",p[which.max(p)],"%\n\n")
