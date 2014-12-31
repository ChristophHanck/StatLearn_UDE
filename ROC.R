library(rpart)
library(ROCR)
library(MASS)
library(ISLR)

#generate some data
N_train = 2000
N_test = 10000
df = 10
split = qchisq(.5,df)
p = 10
X = matrix(rnorm((N_train+N_test)*p),ncol=p)
Y = (rowSums(X^2)>split)
dataYX=data.frame(Y,X)
m1.logit <- glm(Y ~ ., family = binomial(link = "logit"), data = dataYX[1:N_train,])
#m1.yhat <- predict(m1.logit, newdata=dataYX[-(1:N_train),], type = "response")
m1.yhat <- predict(m1.logit,type = "response")
m1.scores <- prediction(m1.yhat, dataYX[1:N_train,1])
plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

lda.fit=lda(Y~.,data=dataYX[1:N_train,])
lda.yhat <- predict(lda.fit)
lda.scores <- prediction(lda.yhat$posterior[,2], dataYX[1:N_train,1])
plot(performance(lda.scores, "tpr", "fpr"), col = "gold",lwd=2,add=TRUE)

attach(Smarket)

lda.fit=lda(Direction~Lag1+Volume,data=Smarket)
lda.yhat <- predict(lda.fit)
lda.scores <- prediction(lda.yhat$posterior[,2], Direction)
plot(performance(lda.scores, "tpr", "fpr"), col = "gold",lwd=2)
abline(0,1, lty = 8, col = "grey")

glm.fit=glm(Direction~Lag1+Volume,data=Smarket,family="binomial")
m1.yhat <- predict(glm.fit,type="response")
m1.scores <- prediction(m1.yhat, Direction)
plot(performance(m1.scores, "tpr", "fpr"), col = "purple",lwd=2,add=TRUE)

qda.fit=qda(Direction~Lag1+Volume,data=Smarket)
qda.yhat <- predict(qda.fit)
qda.scores <- prediction(qda.yhat$posterior[,2], Direction)
plot(performance(qda.scores, "tpr", "fpr"), col = "lightblue",lwd=2,add=TRUE)

cor(m1.yhat,lda.yhat$posterior[,2]) 
max(as.numeric(m1.yhat)-as.numeric(lda.yhat$posterior[,2])) # only almost exactly identical!
# most likely, predicted probabilities are so similar that classification differ at virtually not cutoff, leading to basically the same ROC curve for many datasets
