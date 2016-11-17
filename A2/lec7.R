# Lecture 7

# Table 3.7.  
# X = days of training received; 
# Y= Sales Performance Score
X = c(0.5, 0.5, 1,1,1.5,1.5,2,2,2.5,2.5)
Y=c(42.5,50.6,68.5,80.7,89.0,99.6,105.3,111.8,112.3,125.7)

# square root transformation of X
Xp = sqrt(X)
# logarithrim transformation of X
Xlog = log(X)

fit0=lm(Y~X)           # fit to original data
fit1=lm(Y~Xp)         # square root transformation of X
fit2=lm(Y~Xlog)   # logarithrim transformation of X

par(mfrow=c(2,2))
plot(Y~X, type="p",col="red",main="Before transformation of X")
plot(Y~Xp,type="p",,col="red",xlab=expression(paste(sqrt(X))),
      main="After transformation of X" )
plot(fit1,1,main="After transformation of X")
plot(fit1,2,main="After transformation of X")

## compare sqrt(X) and log(X) 
par(mfrow=c(2,2))
plot(fit1, 1); plot(fit1,2)
plot(fit2,1); plot(fit2,2)

## compare SSE and R-squared
anova(fit0)
anova(fit1)
anova(fit2)

# CI and PI
# default level =0.95
confint(fit1)
predict.lm(fit1,newdata=data.frame(Xp=1.2), interval="confidence")
predict.lm(fit1,newdata=data.frame(Xp=c(1,1.2), interval="prediction")


#----------------------------------------------------------
# Analysis of US GNP data (1947-1997)
#----------------------------------------------------------

data = read.table(file.choose(),sep=",")
GNP=data[,2]
year=data[,1]

plot(GNP~year, ylab="GNP( $Billions)",type="p",pch=20,col="red")

# M0: GNP~year 
M0 = lm(GNP~year)
par(mfrow=c(1,3))
plot(GNP~year, ylab="GNP( $Billions)",type="p",pch=20,col="red")
abline(M0,col="blue")
plot(year,M0$res,type="b")
plot(M0,2)

# M1: log(Y)~year

logGNP=log(GNP)
time =year-1947
M1 = lm(logGNP~time)
par(mfrow=c(1,3))
plot(logGNP~time, ylab="log(GNP)( $Billions)",type="p",pch=20,col="red")
abline(M1,col="blue")
plot(time, M1$res,type="b")
plot(M1,2)

# M2: sqrt(Y)~year

sqrtGNP=sqrt(GNP)
time =year-1947
M2 = lm(sqrtGNP~ time)
par(mfrow=c(1,3))
plot(sqrtGNP~ time, ylab="sqrt(GNP)( $Billions)",type="p",pch=20,col="red")
abline(M2,col="blue")
plot(time,M2$res,type="b")
plot(M2,2)

## SSE
m0sse= sum( (GNP-M0$fitted)^2)
m1sse= sum( (GNP-exp(M1$fitted))^2)
m2sse= sum( (GNP-(M2$fitted)^2)^2)

# prediction based on M1
confint(M1) # log(GNP)~ (year-1947)
# time=64 ->  1947+50=year 1997; 1947+63= year 2010,  
predict.lm(M1,newdata=data.frame(time=50),interval="confidence",level=0.90)
exp(predict.lm(M1,newdata=data.frame(time=50),interval="confidence",level=0.90))
predict.lm(M1,newdata=data.frame(time=63),interval="prediction",level=0.95)
exp(predict.lm(M1,newdata=data.frame(time=63),interval="prediction",level=0.95))

# word recall experiment 
word = read.table(file.choose(),header=T)
t(word)
logtime = log(word$time)

par(mfrow=c(1,2))
plot(word$time,word$prop,xlab="Time",ylab="Prop", type="p",col="blue",
        main="Before transformation on X")
abline(lm(prop~time,data=word),col="red")
plot(logtime,word$prop,xlab="log(Time)",ylab="Prop", type="p",col="blue",
        main="log(X)" )
abline(lm(word$prop~logtime),col="red")

# check residual and normality 
par(mfrow=c(2,2))
plot(lm(prop~time,data=word),1,main="Y~X")
plot(lm(prop~time,data=word),2,main="Y~X")
plot(lm(word$prop~logtime),1,main="Y~log(X)")
plot(lm(word$prop~logtime),2,main="Y~log(X)")



#----------------------------------------------------------
# Box-Cox transformation: 
#      GPA and ACT score example
#----------------------------------------------------------

data <- read.table(file.choose(), header = F)
y <- data[,1]
x <- data[,2]

plot(x, y)
reg <- lm("y ~ x")
abline(reg)

resSS <- function(x, y, lambda){
  n <- length(y)
  k2 <- (prod(y))^(1/n)
  k1 <- 1/(lambda * (k2^(lambda - 1)))
  w <- rep(NA, n)
  for(i in 1:n){
    w[i] <- ifelse(lambda == 0, (k2 * log(y[i])), (k1 * (y[i]^lambda - 1)))
  }
  reg_fit <- lm(w ~ x)
  SSE <- deviance(reg_fit)  # deviance(lm(y~x))= SSE
  return(SSE)
}

lambda = seq(0, 6, by = 0.01)
SSE <- rep(NA, length(lambda))

for(i in 1:length(lambda)){
  SSE[i] <- resSS(x, y, lambda[i])
}

plot(lambda, SSE, type = "l", lty = 1, lwd = 4, 
     xlab = expression(lambda), ylab = "SSE", main="Box-Cox transformation: GPA vs ACT score example")
points(lambda[which.min(SSE)],min(SSE),pch=20,col="red",lwd=2)
lambda[which.min(SSE)]

## using boxcox() in R package MASS
#
library(MASS) 
bc=boxcox(y~x, lambda = seq(-2, 4, 0.01))
lambdahat= bc$x[which.max(bc$y)]
lambdahat

#----------------------------------------------------------
# Joint Inference : 
#      Toluca data example
#----------------------------------------------------------
toluca=read.table(file.choose(),col.names = c("lotsize", "workhrs"))
# plot(toluca$lotsize,toluca$workhrs)

modt = lm(lotsize~workhrs,data=toluca)
confint(modt)
confint(modt,level=1-0.05/2) # Bonferroni C.I.


