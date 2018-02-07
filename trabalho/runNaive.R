source("lerDados")
source("naiveBayes")
source("amostragem")

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

dados=random(titanic,1)
naive.bayes(dados,1)

#source
rotulo=c()
for(i in 1:nrow(dados$test)){
  a = naive.bayes(dados,i)
  rotulo[i]=names(which.max(a))
}
rotulo=as.integer(rotulo)
prob_modelo = sum(rotulo==dados$testRotulo)/length(dados$testRotulo)
cat("Modelo com acertivas de ",prob_modelo*100,"% da base de teste\n")
