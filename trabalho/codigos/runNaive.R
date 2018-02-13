source("codigos/lerDados")
source("codigos/naiveBayes")
source("codigos/amostragem")


#set.seed(62564662) # 75.11%
#set.seed(37298608) # 78%

source("codigos/tratamento")

for(i in 1:ncol(titanic)){
  titanic[,i]=as.factor(titanic[,i])
}
cat("Executando Naive Bayes, para os 297 casos de teste\n")
set.seed(37298608)
dados=random(titanic,1)
naive.bayes(dados,1)

#source
rotulo=c()
for(i in 1:nrow(dados$test)){
  a = naive.bayes(dados,i)
  rotulo[i]=names(which.max(a))
}
rotulo=as.integer(rotulo)
VP = sum(dados$testRotulo==1 & rotulo==1)
FP = sum(dados$testRotulo==1 & rotulo==0)
VN = sum(dados$testRotulo==0 & rotulo==0)
FN = sum(dados$testRotulo==0 & rotulo==1)
Verdadeiros=sum(rotulo==dados$testRotulo)
prob_modelo = Verdadeiros/length(dados$testRotulo)
cat("\tQuantidade de acertos: ",Verdadeiros,"\n")
cat("\tModelo com acertivas de ",prob_modelo*100,"% da base de teste\n")
#Calculando a matriz Confusão
M=matrix(c(VP,FN,FP,VN),nrow=2)
cat("Matriz de Confusão\n")
colnames(M) <- c("+","-")
rownames(M) <-c("+","-")
print(M)
TFP_NB=FP/(FP+VN)
TVP_NB=VP/(VP+FN)
