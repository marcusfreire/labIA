cat("\t\t\t PC1 tPC2\n")
cat("Desvio Padrao\t",sqrt(var(compc[,1])),"\t")
cat("Propocao de Variancia\t", cov(compc)[1,1]/sum(diag(cov(compc))),"\t", cov(compc)[2,2]/sum(diag(cov(compc))),"\n")
