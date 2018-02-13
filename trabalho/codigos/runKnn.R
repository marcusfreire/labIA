source("codigos/lerDados")
source("codigos/amostragem")
source("codigos/knn")
#install.packages("zoo")
require("zoo")
#-----------------Tratando Base ---- Para KNN --
titanic=titanic[-3]

temp=as.numeric(titanic$Age)
temp=abs(na.spline(temp))
temp=scale(temp)
titanic$Age=temp[,1]

temp=as.numeric(titanic$Fare)
temp=scale(temp)
titanic$Fare=temp[,1]

#--------- Inicio Knn
set.seed(40601223)
dados=random(titanic,1)
cat("Executando Knn, para os 20 primeiros casos de teste\n")
rotulo=c()
rotulo=knn(dados,20)
VP = sum(dados$testRotulo[1:20]==1 & rotulo==1)
FP = sum(dados$testRotulo[1:20]==1 & rotulo==0)
VN = sum(dados$testRotulo[1:20]==0 & rotulo==0)
FN = sum(dados$testRotulo[1:20]==0 & rotulo==1)
#Calculando a matriz Confusão
M=matrix(c(VP,FN,FP,VN),nrow=2)
Verdadeiros = sum(dados$testRotulo[1:20]==rotulo)
Verd_percentual = Verdadeiros/20
cat("\tQuantidade de acertos: ",Verdadeiros,"\n")
cat("\tPorcentagem de verdadeiros sobre os testes: ",Verd_percentual*100,"%\n")
cat("Matriz de Confusão\n")
colnames(M) <- c("+","-")
rownames(M) <-c("+","-")
print(M)
TFP_KNN=FP/(FP+VN)
TVP_KNN=VP/(VP+FN)
