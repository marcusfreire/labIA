source("lerDados")
source("amostragem")
source("knn")

#Para um teste de 20 amostras
#set.seed(124578) # 75%
#set.seed(1946827350) # 90% acertos
#Para todos os testes Nº Aleatórios:
# 738638014 71%

dados=random(titanic,1)
cat("Executando Knn, para os 20 primeiros casos de teste\n")
rotulo=c()
rotulo=knn(dados,20)
Verdadeiros = sum(dados$testRotulo[1:20]==rotulo)
Verd_percentual = Verdadeiros/20
cat("Quantidade de acertos: ",Verdadeiros,"\n")
cat("Porcentagem de verdadeiros sobre os testes: ",Verd_percentual,"\n")
