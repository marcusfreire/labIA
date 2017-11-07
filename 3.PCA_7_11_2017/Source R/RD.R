matriz=matrix(c(1,2,3,4,1,4,9,16),4,2)
trans%*%matriz


matriz2=matrix(c(1,4,9,16,1,2,3,4),4,2)
trans%*%matriz2

M1=matriz%*%trans
M2=matriz2%*%trans

em1=eigen(M1)
em2=eigen(M2)

em1$values
em2$values

em1$vectors
em2$vectors

irs=iris[,1:4]
irs2=iris[,4:1]

irs=scale(irs,center=T)
irs2=scale(irs2,center=T)

head(irs,3)
head(irs2,3)

evdirs=eigen(t(irs)%*%irs)
evdirs2=eigen(t(irs2)%*%irs2)

values = evdirs$values / (nrow(irs) - 1)
values2 = evdirs2$values / (nrow(irs2) - 1)

head(evdirs$vectors,3)
head(evdirs2$vectors,3)

Covariancia  <-  function(y)
{
   y = as.matrix(y)
   n = nrow(y)
   p = ncol(y)
   if(p>1){ 
      x = apply(y, 2, mean) 
      w = array(dim=(c(n, p, p)))
      w.mean=array(dim=c(p,p))
      var.s=array(dim=c(p,p))
      s=array(dim=c(p,p))
      for (k in 1:n){
         for (i in 1:p){
            for (j in 1:p){
               w[k,i,j] = (y[k,i] - x[i])*(y[k,j] - x[j])
            }
         }
      }
      w.mean=array(dim=c(p,p))
      for (i in 1:p){
         for (j in 1:p){
            w.mean[i,j] = sum(w[,i,j])/n
         }
      }
      s = w.mean*n/(n-1)
      for (i in 1:p){
         for (j in 1:p){
            var.s[i,j] = sum((w[,i,j] - w.mean[i,j])*(w[,i,j] - w.mean[i,j]))*n/((n-1)*(n-1)*(n-1))
         }
      }
      sum.var = sum(var.s) - sum(diag(var.s))
      sum.s2 = sum(s*s) - sum(diag(s)*diag(s))
      lamb = sum.var/sum.s2
      if (lamb > 1){lamb = 1}
      if (lamb < 0){lamb = 0}
      s.star = s*(1-lamb)
      s.star[row(s.star)==col(s.star)] = s[row(s)==col(s)]
      var.y = s.star
   }
}

finaldate=t(evdirs$vectors[,1])%*%t(irs)
#finaldate é o p score, em prcomp?

