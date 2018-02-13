#Atenção, este código tem uma dependência
#Deverá instalar o pacote zoo
#install.packages("zoo")
#Execute no R, source("run.R")

source("codigos/runKnn.R")
cat("\n")
source("codigos/runNaive.R")
cat("\n")
source("codigos/runRNA.R")
#Análise ROC

plot( 0, type="n", xlim=c(0,1), ylim=c(0,1),main="Análise ROC",xlab="Taxa de Falsos Positivos",ylab="Taxa de Verdadeiros Positivos" )

points(TFP_NB,TVP_NB, col="blue", pch=15)
points(TFP_KNN,TVP_KNN, col="purple", pch=15 )
points(TFP_RNA,TVP_RNA, col="red", pch=15 )
abline(a=0,b=1,lty=2)
legend( x=0.4,y=0.2,
        legend=c("NaiveBayes","KNN","RNA"),
        col=c("blue","purple","red"),lwd=1,
        pch=c(15,15,15) )
