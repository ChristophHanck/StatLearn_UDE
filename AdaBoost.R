rm(list=ls())
library(mvtnorm)
library(rpart)
library(scales)

#set.seed(1)

N1 = 150
N2 = 150
K = 2

mu1 = c(-1,3)
mu2 = c(2,0)

cov1 = -0.5
v11 = 2
v12 = 2
Sigma1 = matrix(c(v11,cov1,cov1,v12),nrow=2)

cov2 = 0.4
v21 = 2
v22 = 2
Sigma2 = matrix(c(v21,cov2,cov2,v22),nrow=2)

x1 = rmvnorm(N1,mu1,Sigma1)
x2 = rmvnorm(N2,mu2,Sigma2)


plot(x1[,1],x1[,2],col="red",xlim=c(-4,5),ylim=c(-3,5),pch="+")
points(x2[,1],x2[,2],col="blue",pch="x")

X<-rbind(x1,x2)
Y<-c(rep(TRUE,150),rep(FALSE,150))
dataYX=data.frame(Y,X)

weights = rep(1/(N1+N2),(N1+N2)) # initial weights
M=5
err = rep(NA,M) 
alpha = rep(NA,M) 
K1seq=seq(-4,5,by=.1)
K2seq=seq(-4,5,by=.1)
grid<-as.data.frame(expand.grid("X1"=K1seq,"X2"=K2seq))

G = array(rep(NA,length(K1seq)*length(K2seq)*M),c(length(K1seq),length(K2seq),M))
point_size<-rep(1,(N1+N2))
for (m in 1:M)
 {
 fit = rpart(Y~.,weights=weights,method="class",maxdepth=1,data=dataYX,xval=0,control=rpart.control(minbucket=1,maxdepth=1,cp=-1))
 G[1:length(K1seq),1:length(K2seq),m]<-predict (fit,newdata=grid,type="class")
if (substring(labels(fit)[2],1,2)=="X1"){
	segments(fit$splits[1,4],-3,fit$splits[1,4],5)
	}else
	{
		segments(-4,fit$splits[1,4],5,fit$splits[1,4])
		}
	
	if(fit$frame$complexity[1]<=1e-05){title(main="Split does not decrease the overall lack of fit")}
locator(1)
misclassified<-predict (fit,type="class")!=Y
points(X[misclassified,],cex=3)
corclassified<-predict (fit,type="class")==Y
locator(1)
err = sum(weights*misclassified)/sum(weights)
 alpha[m] = log((1-err)/err)
 weights = weights*exp(alpha[m]*misclassified)

point_size<-point_size+misclassified*exp(alpha[m])*0.2#sqrt(match(weights,sort(unique(weights))))/2

	
	plot(x1[1,1],x1[1,2],col="red",xlim=c(-4,5),ylim=c(-3,5),pch="+",cex=point_size[1])
#points(x2[corclassified[(N1+1):(N1+N2)],1],x2[corclassified[(N1+1):(N1+N2)],2],col="blue",pch="x")

for (a in 2:(N1+N2))
 {
	if (a<=N1){
		points(X[a,1],X[a,2],col="red",xlim=c(-4,5),ylim=c(-3,5),pch="+",cex=point_size[a])}
	if (a>N1){
		points(X[a,1],X[a,2],col="blue",xlim=c(-4,5),ylim=c(-3,5),pch="x",cex=point_size[a])}
}
locator(1)

}
 G[G==1]<--1
 G[G==2]<-1


F<-G[,,1]*alpha[1]+G[,,2]*alpha[2]+G[,,3]*alpha[3]+G[,,4]*alpha[4]+G[,,5]*alpha[5]
plot(x1[,1],x1[,2],col="red",xlim=c(-4,5),ylim=c(-3,5),pch="+")
points(x2[,1],x2[,2],col="blue",pch="x")
contour(K1seq,K2seq,F,levels=0,add=TRUE)
locator(1)
plot(x1[,1],x1[,2],col=alpha("red",0.15),xlim=c(-4,5),ylim=c(-3,5),pch="+")
points(x2[,1],x2[,2],col=alpha("blue",0.15),pch="x")
contour(K1seq,K2seq,F,levels=0,add=TRUE)
locator(1)
TestN<-40
Testx1 = rmvnorm(TestN,mu1,Sigma1)
Testx2 = rmvnorm(TestN,mu2,Sigma2)

Testx1p<-Testx1
Testx2p<-Testx2

points(Testx1[,1],Testx1[,2],col="red",xlim=c(-4,5),ylim=c(-3,5),pch="+")
points(Testx2[,1],Testx2[,2],col="blue",pch="x")
locator(1)
classified_x1<-rep(NA,TestN)
classified_x2<-rep(NA,TestN)
#shift poits outside the classification grid on edge for classification 
Testx1[which(Testx1[,1]>5),1]<-5
Testx1[which(Testx1[,1]<(-4)),1]<--4
Testx1[which(Testx1[,2]>5),2]<-5
Testx1[which(Testx1[,2]<(-4)),2]<--4
Testx2[which(Testx2[,1]>5),1]<-5
Testx2[which(Testx2[,1]<(-4)),1]<--4
Testx2[which(Testx2[,2]>5),2]<-5
Testx2[which(Testx2[,2]<(-4)),2]<--4


for (n in 1:TestN){
	 classified_x1[n]<-F[which(round(Testx1[n,1],1)==round(K1seq,1)),which(round(Testx1[n,2],1)==round(K2seq,1))]<0
	 classified_x2[n]<-F[which(round(Testx2[n,1],1)==round(K1seq,1)),which(round(Testx2[n,2],1)==round(K2seq,1))]>0
}

points(Testx1p[classified_x1,1],Testx1p[classified_x1,2],cex=3)
points(Testx2p[classified_x2,1],Testx2p[classified_x2,2],cex=3)

Test_Error<-sum(classified_x1,classified_x2)/(2*TestN)
locator(1)
title(main=c("Test Error: ",Test_Error))