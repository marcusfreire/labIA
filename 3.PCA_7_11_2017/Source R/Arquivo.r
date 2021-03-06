#EVD
A = matrix(c(4,2,5,1),nrow=2)
#Matriz Simétrica
A.simetrica = t(A) %*% A
# Decomposição de AutoValores
EVD = eigen(A.simetrica)
#AutoValores
lambda = EVD$values
#AutoVetores
U = EVD$vectors

#SDV

set.seed(20)
M = matrix(rnorm(9),3,3)
#Matriz Simétrica
> Y = t(M) %*% M
# Decomposição de valores singulares
SVD = svd(Y)
# Orthornomal de U
t(SDV$u) %*% SDV$u
# Ortornormal de V
t(SDV$v) %*% SDV$v
# Y é igual U D V'
U %*% diag(SDV$d) %*% t(V)


# Sabemos que, se U= t(U) %*% U, então Y = U %*% diag(lambda) %*% t(U)


M=matrix(c(1,-2,-3,-2,3,-1,3,-1,2),3,3)
#Matriz inversa
Mi=solve(M)
#AutoValores
eigen(M)$values
#AutoVetores
eigen(M)$vectors
