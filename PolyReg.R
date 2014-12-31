# Polynomial Regression
rm(list=ls())
dev.off()

N=10 # will often not even work due to numerical instabilities for degree 9 when N = 10
#N=100 # much smoother

sin_coeff = 1.5
u = rnorm(N,0,.25)
x = sort(runif(N))
y = sin(sin_coeff*pi*x)+u

degree = 8

plot(x,y,ylim=c(-2,2),pch=16,cex=2,col="blue")
v=locator(1)
xax = seq(0,1,by=0.01)
lines(xax,sin(sin_coeff*pi*xax),lwd=3,type="b")
v=locator(1)
polyfits <- TrainErrors <- vector("list", degree)
legend('topright', paste("degree",1:degree,sep = " "), lty=1, col=2:(degree+1), bty='n', cex=.75)
for(i in 1:degree) 
{ 
  nam <- paste("fit",i,sep = "")
  assign(nam, lm(y~poly(x,i,raw=TRUE)))  
  v=locator(1)
  
  lines(xax, predict(get(paste("fit",i,sep = "")), data.frame(x = xax)), col=i+1,lwd=2)
  
  polyfits[[i]] <- lm(y~poly(x,i,raw=TRUE))
  TrainErrors[[i]] <- var(y-predict(polyfits[[i]]))  
  
  #nam2 <- paste("pol",i,sep = "")
  #assign(nam2, function(x) matrix(x^rep(0:i,each=length(x)),ncol=i+1)%*%get(paste("fit",i,sep = ""))$coefficient[1:(i+1)])
}

#fit1 <- lm(y~poly(x,1,raw=TRUE))
#pol3 <- function(x) fit3$coefficient[4]*x^3 + fit3$coefficient[3]*x^2 + fit3$coefficient[2]*x + fit3$coefficient[1]
#pol9 <- function(x) fit9$coefficient[10]*x^9 + fit9$coefficient[9]*x^8 + fit9$coefficient[8]*x^7 + fit9$coefficient[7]*x^6 + fit9$coefficient[6]*x^5 + fit9$coefficient[5]*x^4 + fit9$coefficient[4]*x^3 + fit9$coefficient[3]*x^2 + fit9$coefficient[2]*x + fit9$coefficient[1]
v=locator(1)

# for test error
rep=100
results = matrix(NA,nrow=rep,ncol=degree)
N_new = N

# it might also make sense to simulate (average) over training fits - else, perforamce hinges on how crazy the fitted model for the single training sample is
for(j in 1:rep)
{
u_new = rnorm(N_new,0,.25)
x_new = sort(runif(N_new))
y_new = sin(sin_coeff*pi*x_new)+u_new

TestErrors <- TestFits <- vector("list", degree)
for(i in 1:degree) # messy with assign, see http://grokbase.com/t/r/r-help/10c4cmyf8y/r-problem-storing-lm-model-in-a-list
{   
  TestFits[[i]] <- predict(polyfits[[i]],newdata=data.frame(x=x_new))
  TestErrors[[i]] <- var(y_new-predict(polyfits[[i]],newdata=data.frame(x=x_new)))  
  }
results[j,] = unlist(TestErrors)
}
TestMSEs = colMeans(results)

par(mfrow=c(2,1))
plot(1:degree,TestMSEs,type="b",col="red",ylim=c(0,max(TestMSEs)))
points(which.min(TestMSEs),TestMSEs[which.min(TestMSEs)],pch=16,col="red")
plot(1:degree,unlist(TrainErrors),type="b",col="blue")
