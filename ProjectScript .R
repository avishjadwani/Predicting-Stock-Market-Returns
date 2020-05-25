install.packages("tseries")
install.packages("xts")
install.packages("candlesticks", repos="http://R-Forge.R-project.org")
install.packages("quantmod")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("plotly")
install.packages("TTR")
install.packages("randomForest")
install.packages("DMwR")
# 
library(tidyverse)
library(dplyr)
library(plotly)
library(TTR)
library(randomForest)
library(quantmod)
library(xts)
library(tseries)
library(candlesticks)
library(quantmod)
library(DMwR)
# In R libraries xts and zoo are very useful for handling time series data
# Getting the stock price data from he yahoon finance website. 
# tseres library includes function get.hist.quote() that can be used
# to download the quotes into a zoo object
# for our project we are taking data from 02 jan 1970 till 15 sep 2009
?get.hist.quote
GSPC<-as.xts(get.hist.quote(instrument = "^GSPC", start= "1970-01-02",end = "2019-08-10",
                      quote = c("Open", "High", "Low", "Close", "Volume", "Adjusted" ),
                      provider = c("yahoo"), method = NULL,origin = "1899-12-30", 
                      compression = "d", retclass = c("zoo","ts"), quiet = FALSE, drop = FALSE))
head(GSPC)
# What to predict ?
# we are defining a variable called tendency which is a measure 
# of confidence that the change in price will be higher than 
# some predefined change in price (P%) in next k days
# The value of this idicator should be realted to the confidence 
# we have that the target margin p will be attainable in next k days

# high + T value= Buy = there will be more occurences of avearage 
#                   daily price clearly above the target variation
# high -ve T value = Sell = there will be more occurence of avg price 
#                     below the taret variation 
#  T= 0 = hold = Market is flat or by conflicting + and - variations
#               that cancel each other 

# Defining T indicator
T.ind = function(quotes, tgt.margin = 0.025, n.days =10){
  v<-apply(HLC(quotes),1,mean)
  #v<-HLC_Average(GSPC)
  r <-matrix(NA,ncol= n.days,nrow = NROW(quotes))
  for (x in 1:n.days) r[,x]<-Next(Delt(v,k=x),x)
  x<-apply(r,1,function(x) sum(x[x>tgt.margin | x< -tgt.margin]))
  if(is.xts(quotes))
    xts(x,time(quotes))
  else x
}
# v = we first begin by extracting values from the quotes object using HLC()
# we calculate the mean price ie. sum(high, low, close)/3
# r = we are creating a matrix num of cols = 10 and num of rows = nrow in entire dataset
?Next() 
# Next(x = series,k = period to advance) function is used to shift hte value of time series 
#Create a new series with all values advanced forward one period. 
#The value of period 1, becomes the value at period 2, value at 2 
#becomes the original value at 3 and so on 
?Delt()
# Delt() - used to calculate percenage difference within one series or 
# between two sereies. Delt(x1, k  ) x1 = vector, k = change over k perids
# T.ind fuction returns the sum of changes higher than the predefined chage 
#head(T.ind(GSPC))
# ##
#  Ttest = function(quotes, tgt.margin = 0.025, n.days =10){
#  
#   v<-HLC_Average(quotes)
#    r <-matrix(NA,ncol= n.days,nrow = NROW(quotes))
#    for (x in 1:n.days) r[,x]<-Next(Delt(v,k=x),x)
#    return(r)
#  }
#  test = Ttest(GSPC)
#  head(test)
# head( HLC_Average(GSPC))
# The Ttest fucntion returns the percentage change in price stock 
# ie. when x = 1,  (price day 2 - price day 1)/priceday1
# when x = 2, (price day 3- price day 1)/ price day 1 
head(T.ind(GSPC))
#The fifth row = -.12342 means high tendency that there will be dip in market 

# Creating candelsticks graph
threemonths <- xts::last(GSPC, "3 months")
candleChart(xts::last(GSPC, "3 months"), theme = "white", TA = NULL)
avgPrice <- function(p) apply(HLC(p), 1, mean)
addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")
addT.ind <- newTA(FUN = T.ind, col = "red", legend = "tgtRet")
addAvgPrice(on = 1)
addT.ind()
# We are plotitng candelstick and Tendency plot to see how better our 
# indicator is 
# we can see that our T idicator reaches its highest value 
# when there is subsequent period of positive variation 
###############################################################################
# selecting Technical indicators to predict Tendency 
# we select some most common  technical indicators and use random forest
# to see the importance of each model, which can be estimated by calculating the percentage
# increase in error of the random forest if we remove each variable in turn. 
# if theres is high/ significant change we will keep that variable otherwise we will 
# remove it
 
# Before doing random forest we have to first calculate the technical indicators which
# can be done using TTR function. These variables include MACD, SMI, ADX, Bollinger Bands etc/
library(TTR)
myATR <- function(x) ATR(HLC(x))[, "atr"]
#myATR is a function that calculates ATR of quotes data and store it in a column named atr
# similarily others:
mySMI <- function(x) SMI(HLC(x))[, "SMI"]
myADX <- function(x) ADX(HLC(x))[, "ADX"]
myAroon <- function(x) aroon(x[, c("High", "Low")])$oscillator
myBB <- function(x) BBands(HLC(x))[, "pctB"]
myChaikinVol <- function(x) Delt(chaikinVolatility(x[, c("High","Low")]))[, 1]
myCLV <- function(x) EMA(CLV(HLC(x)))[, 1]
myEMV <- function(x) EMV(x[, c("High", "Low")], x[, "Volume"])[,2]
myMACD <- function(x) MACD(Cl(x))[, 2]
myMFI <- function(x) MFI(x[, c("High", "Low", "Close")], x[, "Volume"])
mySAR <- function(x) SAR(x[, c("High", "Close")])[, 1]
myVolat <- function(x) volatility(OHLC(x), calc = "garman")[, 1]
# Creating a Random forest now 
??data()
#data(GSPC)
library(randomForest)
#we will specify a model and then feed it into a RF model 
#########$$$$$$
data(GSPC)


##
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) +
                             myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) +
                             myBB(GSPC) + myChaikinVol(GSPC) +  myCLV(GSPC)+ CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC)))+myEMV(GSPC)+myVolat(GSPC) +myMACD(GSPC)+myMFI(GSPC)+
                             RSI(Cl(GSPC)) + mySAR(GSPC)+ runMean(Cl(GSPC))  +  runSD(Cl(GSPC)) 
                             
)


#######$$$$$$
#data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) + myATR(GSPC)  +  mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) + myBB(GSPC) + myChaikinVol(GSPC) + myCLV(GSPC) + CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) + myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) + mySAR(GSPC) + runMean(Cl(GSPC)) +  runSD(Cl(GSPC)))
set.seed(1234)
rf <- buildModel(data.model,method='randomForest',
                     training.per=c(start(GSPC),index(GSPC["1999-12-31"])),
                     ntree=50, importance=T) 
# We have mentioned importance = TRUE so that the radom forest estimates the variable importance 
# It is calcualtes the percentage increase in errror of rhe forest if we remove each variable in turn 

summary(rf)
# after constructin the model we can check the importance of the variable using varImpPlot
varImpPlot(rf@fitted.model, type = 1)
# we have to select a threshold value to limit the variables
# our threshold value = 10
imp<-importance(rf@fitted.model,type =1 )
rownames(imp)[which(imp>10)]
# Based on the results above we can select the final model variabless
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC), k = 1) +
                             myATR(GSPC) + myADX(GSPC) + myEMV(GSPC) + myVolat(GSPC) +
                             myMACD(GSPC) + mySAR(GSPC) + runMean(Cl(GSPC)))

######
#Dividing data into training and evaluation data
# selecting training data
nrow(GSPC)
Tdata.train <- as.data.frame(modelData(data.model, data.window=c('1970-01-02','2010-12-31')))
# selecting evaluation data which will not be used to build models
# we will use na.omit function to remove rows with na vlues
nrow(Tdata.train)
Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window=c('2011-01-01','2019-08-10'))))
nrow(Tdata.eval)
Tform <- as.formula('T.ind.GSPC ~ .')

# Artificial Neural Network (ANN )
# first we create a Regression ANN model with 1 output
install.packages("nnet")
library(nnet)
# Normalizing data using scale funcion 
norm.data<-scale(Tdata.train)
# building ANN model size = num of hidden nodes, decay controls the weight updating rate of 
# back propogation ,  maxit = max num of iterations 
nn<-nnet(Tform,norm.data[1:8000,],size = 3,decay = 0.01,maxit = 1000 , linout = T, trace = F)
# doing prediction 
norm.preds <- predict(nn, norm.data[8001:10316, ])
# Rescaling values
preds <- unscale(norm.preds, norm.data)
# to evaluate the signals 
# trading.signals transform numeric predictions into signals given 
# buy sell threshold
sigs.nn<-trading.signals(preds,0.1,-0.1)
true.sigs<-trading.signals(Tdata.train[8001:10316, "T.ind.GSPC"],0.1,-0.1)
#sigs.PR() obtains a matrix with the preciosion and recall scores of the two 
# type of events
sigs.PR(sigs.nn, true.sigs)
#################################################################################################
# Plotting results in a candelstick chart 

first(Tdata.train[8001:10316,])

# data_train2<-GSPC[8034:10349,]
# last(data_train2)
data_train2<-window(GSPC,start="2001-10-22",end="2010-12-31")

candleChart(xts::last(data_train2, "3 months"), theme = "white", TA = NULL)
avgPrice <- function(p) apply(HLC(p), 1, mean)
addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")
addT.ind <- newTA(FUN = T.ind, col = "red", legend = "tgtRet")
addAvgPrice(on = 1)
addT.ind()

# 
addpredT <- addTA(ta=preds,col="blue", legegend = "Predicted T",)
addpredT

#################################################################################################
# Predicting T values for  validation data 
norm.data_eval<-scale(Tdata.eval)
norm.preds_eval <- predict(nn, norm.data_eval)
preds_eval <- unscale(norm.preds_eval, norm.data_eval)

sigs.nn_eval<-trading.signals(preds_eval,0.1,-0.1)
true.sigs_eval<-trading.signals(Tdata.eval[, "T.ind.GSPC"],0.1,-0.1)

sigs.PR(sigs.nn_eval, true.sigs_eval) 
######################################################################
# Plotting candelstick for validation data
first(Tdata.eval)
last(Tdata.eval)
# data_train2<-GSPC[8034:10349,]
# last(data_train2)
data_eval<-window(GSPC,start="2011-01-03",end="2019-07-26")

candleChart(xts::last(data_eval, "3 months"), theme = "white", TA = NULL)
avgPrice <- function(p) apply(HLC(p), 1, mean)
addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")
addT.ind <- newTA(FUN = T.ind, col = "red", legend = "tgtRet")
addAvgPrice(on = 1)
addT.ind()

# 
addpredT <- addTA(ta=preds_eval,col="blue", legegend = "Predicted T",)
addpredT


#################################################################################################
# Using ANN as classification 
set.seet(1234)
library(nnet)
# trading.signals() creates signal h,b and s using thresholds
signals<-trading.signals(Tdata.train[,"T.ind.GSPC"], 0.1,-0.1)
# we add categorical varable ie. "signal" in a new data frame
norm.data <- data.frame(signals = signals, scale(Tdata.train[,-1]))
nn <- nnet(signals ~ ., norm.data[1:1000, ], size = 3, decay = 0.01,
            maxit = 1000, trace = F)
preds <- predict(nn, norm.data[1001:2000, ], type = "class")
sigs.PR(preds, norm.data[1001:2000, 1])
###########################################################
#Creating another model using SVM 
install.packages("kernlab")
library(kernlab)
data <- cbind(signals = signals, Tdata.train[, -1])
?ksvm
# c = lagrange costant 
ksv <- ksvm(signals ~ ., data[1:1000, ], C = 10)
#Using automatic sigma estimation (sigest) for RBF or laplace kernel
ks.preds <- predict(ksv, data[1001:2000, ])
sigs.PR(ks.preds, data[1001:2000, 1])
############
# Creating model using Multivariate Adapative Regression Splines 
# MARS
install.packages("earth")
library(earth)
e <- earth(Tform, Tdata.train[1:1000, ])
e.preds <- predict(e, Tdata.train[1001:2000, ])
sigs.e <- trading.signals(e.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000, "T.ind.GSPC"],
                               0.1, -0.1)
sigs.PR(sigs.e, true.sigs)
##################################################################















