source("rn")
source("amostragem")

#install.packages("zoo")
require("zoo")
set.seed(1946827350)
#-----------------Tratando Base
titanic=read.csv("dataset/train.csv",head=T)
titanic=titanic[-1]
titanic=titanic[-3]

temp=as.vector(titanic$Sex)
temp[which(temp=="male")]="0"
temp[which(temp=="female")]="1"
titanic$Sex=as.integer(temp)

titanic$Age=abs(na.spline(titanic$Age))

temp=as.vector(titanic$Embarked)
temp[which(titanic$Embarked==""),]=NA
temp[which(temp=="C")]="1"
temp[which(temp=="Q")]="2"
temp[which(temp=="S")]="3"
temp=as.integer(temp)
temp=na.fill(temp,c(1,2,3))
titanic$Embarked=as.integer(temp)

temp=as.integer(titanic$Ticket)
titanic$Ticket=temp

#Excluindo Cabin
titanic=titanic[,-9]
#---FIM-----------Tratando Base

dados=random(titanic,1)
a=rna(dados)
a[[3]][which(a[[3]]==-1)]=0
prob_modelo=sum(a[[3]]==dados$trainigRotulo)/length(dados$trainigRotulo)
cat("Modelo com acertivas de ",prob_modelo*100,"% da base de treinamento\n") #64.98316%
rot=teste(dados,a)
prob_modelo=sum(rot==dados$testRotulo)/length(rot)
cat("Modelo com acertivas de ",prob_modelo*100,"% da base de teste\n") #59.59596%
