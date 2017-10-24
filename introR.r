set.seed(123456)
print(rnorm(10,mean=0,sd=1))
a=1
if(a>2){
	cat("Maior\n")
}else{
	cat("Menor\n")
}

ifelse(a>2,"maior","menor")

for (a in 1:5){
	cat(a,"\n")
}

vet=1:5
vet2=98:100
for (a in c(1:5,vet2)){
cat(a,"\n") 
}

m=matrix(1:10,nrow=5,ncol=2,byrow=T)

