#Import Data 
houses.csv = read.table("monthly-sales-of-new-onefamily-h.csv"
                        , sep=",", header=FALSE, skip=1, nrows=265 )
#Create TS & Plot TS
houses = ts(houses.csv[,2], start = c(1973,1), frequency = 12)
ts.plot(houses,main = "Original data")

#BC Transformation on data
library(MASS)
t = 1:length(houses)
fit = lm(houses ~ t)
bcTransform = boxcox(houses ~ t,plotit = TRUE)

lambda = bcTransform$x[which(bcTransform$y == max(bcTransform$y))]
houses.bc = (1/lambda)*(houses^lambda-1)

#Plot transformed TS
ts.plot(houses,main = "Original data",ylab = expression(X[t]))
ts.plot(houses.bc,main = "Box-Cox tranformed data", ylab = expression(Y[t]))


#Compare Var 
var(houses)
var(houses.bc)


#Plotting ACF/PACF of transformed data 
{op = par(mfrow = c(1,2))
  acf(houses.bc,lag.max = 60,main = "")
  pacf(houses.bc,lag.max = 60,main = "")
  title("Box-Cox Transformed Time Series", line = -1, outer=TRUE)
  par(op)}


{op = par(mfrow = c(1,2))
  #Differencing data at lag 1
  y1 = diff(houses.bc, 1)
  plot(y1,main = "De-trended TS",ylab = expression(nabla~Y[t]))
  
  #Differencing data at lag 12
  y12 = diff(y1, 12)
  ts.plot(y12,main = "De-trended/seasonalized TS",ylab
          = expression(nabla^{12}~nabla~Y[t]))
  par(op)}


#Plot ACF/PACF of differenced data
{op = par(mfrow = c(1,2))
  acf(y12,lag.max = 100,main = "")
  pacf(y12,lag.max = 100,main = "")
  title("De-trended/seasonalized Time Series", line = -1, outer=TRUE)
  par(op)}


#Candidate Models
fit1<-arima(houses.bc, order=c(0,0,0), seasonal=list(order=c(2,0,1), period=12))
fit1

fit2<-arima(houses.bc, order=c(0,0,0), seasonal=list(order=c(4,0,1), period=12))
fit2

fit3<-arima(houses.bc, order=c(2,0,0), seasonal=list(order=c(2,0,1), period=12))
fit3

#Causal/Invertible Fit1
polyroot(c(1, -1.0058, 0.3863  )) #SAR2 for Model 1 
polyroot(c(1, -0.2581 )) #SMA1 for Model 1 


#Causal/Invertible Fit2
polyroot(c(1, -1.5084, 0.5952, 0.2843,-0.3710 )) #SAR4 for Model 2 || 
#Not stationary bc ar root inside unit circle 
polyroot(c( 1, -0.9864)) #SMA1 for Model 2

#Causal/Invertible Fit3
polyroot(c(1, -0.8225,  -0.0435)) #AR2 for Model 3 
polyroot(c(1, -0.3374,-0.2941)) #SAR2 for Model 3
polyroot(c(1, 0.0548 )) #SMA1 for Model 3


#Residuals of first model
res1<-residuals(fit1)
plot(res1, main="Residuals of Model 1")

{op = par(mfrow = c(1,2))
  hist(res1, col="light blue", xlab="", main="Histogram of 
       Residuals")
  qqnorm(res1);qqline(res1)
  par(op)}
mean(res1)
var(res1)

#Diagnostic checking for Model 1
Box.test(res1,lag=16, type = c("Box-Pierce"), fitdf=0)
Box.test(res1,lag=16, type = "Ljung", fitdf=0)
Box.test(res1^2, lag=16, type = "Ljung", fitdf=0) #McLeod-Li/Quadratic correlation test
shapiro.test(res1)

{op = par(mfrow = c(1,2))
  acf(res1)
  pacf(res1)
  par(op)}

#Residuals of second model
res2<-residuals(fit2)
plot(res2, main="Residuals of Model 2")
{op = par(mfrow = c(1,2))
  hist(res2, col="light blue", xlab="", main="Histogram of 
       Residuals")
  qqnorm(res2);qqline(res2)
  par(op)}
mean(res2)
var(res2)

#Diagnostic checking for Model 2
Box.test(res2,lag=16, type = c("Box-Pierce"), fitdf=0)
Box.test(res2,lag=16, type = "Ljung", fitdf=0)
Box.test(res2^2,lag=16, type = "Ljung", fitdf=0)
shapiro.test(res2)

{op = par(mfrow = c(1,2))
  acf(res2)
  pacf(res2)
  par(op)}

#Residuals of third model
res3<-residuals(fit3)
plot(res3, main="Residuals of Model 3")
{op = par(mfrow = c(1,2))
  hist(res3, col="light blue", xlab="", main="Histogram of 
       Residuals")
  qqnorm(res3);qqline(res3)
  par(op)}

mean(res3)
var(res3)

#Diagnostic checking for Model 3
Box.test(res3, lag=16,type = c("Box-Pierce"), fitdf=2)
Box.test(res3, lag=16,type = "Ljung", fitdf=2)
Box.test(res3^2, lag=16,type = "Ljung", fitdf=0)
shapiro.test(res3)
{op = par(mfrow = c(1,2))
  acf(res3)
  pacf(res3)
  par(op)}


#True Points
x.true = c("1995-02","1995-03","1995-04","1995-05","1995-06","1995-07","1995-08","1995-09","1995-10","1995-11")
y.true=c(47,60,58,63,64,64,63,55,54,44)

#Forecasting data points and CI using Model 3
pred <- predict(fit3, n.ahead=10)
newpred<-(2/3*pred$pred+1)^3/2
{ts.plot(houses,xlim=c(1973,1997), ylim=c(0,100), main = "Original data with Forecasts")
  
  space=(1995-1973)/length(houses)
  
  index=2:11*space
  indextoadd=1995+index
  points(indextoadd,pred$pred,type="l",col="red")
  
  lines(indextoadd,pred$pred-1.96*pred$se,lty="dashed")
  lines(indextoadd,pred$pred+1.96*pred$se,lty="dashed")
  points(indextoadd, y.true)
  points(x.true, y.true, pch = "*", col = "blue")}

