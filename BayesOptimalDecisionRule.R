library(MASS)
library(mvtnorm)



N1 = 15
N2 = 15
K = 2

mu1 = c(3,1)
mu2 = c(2,3)

cov1 = 0
v11 = 1
v12 = 1
Sigma1 = matrix(c(v11,cov1,cov1,v12),nrow=2)

cov2 = 0
v21 = 1
v22 = 1
Sigma2 = matrix(c(v21,cov2,cov2,v22),nrow=2)

x1 = rmvnorm(N1,mu1,Sigma1)
x2 = rmvnorm(N2,mu2,Sigma2)

#x1 = matrix(c(1,0,1,0,-1,2),nrow=3)

plot(x1[,1],x1[,2],col="orange",xlim=c(-4,5),ylim=c(-3,5),pch="1",bg="orange")
points(x2[,1],x2[,2],col="lightblue",pch="2",bg="lightblue")

K1seq=seq(-4,4,by=.1)
K2seq=seq(-4,4,by=.1)

delta1 = matrix(NA,nrow=length(K1seq),ncol=length(K1seq))
delta2 = matrix(NA,nrow=length(K1seq),ncol=length(K1seq))

for (i in 1:length(K1seq))
{
  for (j in 1:length(K2seq))
  {
	delta1[i,j]<-sum(exp(rep(-5,N1)*((x1[,1]-rep(K1seq[i],N1))^2+(x1[,2]-rep(K2seq[j],N1))^2)/rep(2,N1)))
	delta2[i,j]<-sum(exp(rep(-5,N2)*((x2[,1]-rep(K1seq[i],N2))^2+(x2[,2]-rep(K2seq[j],N2))^2)/rep(2,N2)))
  }  
}  
contour(K1seq,K2seq,delta1,add=TRUE,col="chocolate",lwd=1.5)
contour(K1seq,K2seq,delta2,add=TRUE,col="blue",lwd=1.5)
contour(K1seq,K2seq,delta1-delta2,levels=0,add=TRUE,col="purple",lwd=3)


color.pal=c("orange","lightblue")
   for (i in 1:length(K1seq))
   {
     for (j in 1:length(K2seq))
     {     
       points(K1seq[i],K2seq[j],col=color.pal[which.max(c(delta1[i,j],delta2[i,j]))],pch=16,cex=.5)
     }
   }


