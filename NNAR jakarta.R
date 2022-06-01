library(neuralnet)
library(forecast)
library(MLmetrics)
library(ggplot2)
# data
library("openxlsx")
datacvd = read.xlsx("C:/Users/ALAMANDA/Downloads/paper ADW/Covid-19 Jakarta.xlsx")
cvdjkt=datacvd[,2];cvdjkt
datejkt=seq(as.Date("2020-03-01"), as.Date("2022-04-03"), by = "day")
covidJKT=ts(cvdjkt, start = c(2020, as.numeric(format(datejkt[1],"%j"))),
         frequency = 365)

# plot data
ggplot(covidJKT, aes(x = datejkt, y = covidJKT)) +
  geom_line(size = 0.2) +
  scale_y_continuous(labels = function(y) format(y, big.mark = ",",
                                                 scientific = FALSE)) +
  labs(x = "Date", y = "Daily Positive Cases")
pacf(covidJKT, main = "Series Daily Positive Cases COVID-19 Jakarta")

#function untuk membuat dataframe target dan lag-lag nya
makeinput = function(x, lags = NULL){
  n = length(x)
  a = matrix(0, nrow = length(x)-lags, ncol = 1+lags)
  a[,1] = x[-c(1:lags)]
  a[,1+lags] = x[-c((n-lags+1):n)]
  for (i in 1:(lags-1)) {
    a[,i+1] = x[-c(1:(lags-i),(n+1-i):n)]
  }
  Ytarget = a[,1]
  Xinput = a[,(2:(lags+1))]
  a = data.frame(Ytarget,Xinput)
  return(a)
}
jktData = makeinput(covidJKT, lags = 27)

##Normalisasi
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
denormalize <- function(xtrans, data = x) {
  return(xtrans*(max(data)-min(data))+min(data))
}
trans = normalize(jktData)

#split data untuk data transformasi
train = head(trans, round(0.8*nrow(trans)))
test = tail(trans, round(0.2*nrow(trans)))

#membuat forecast function
forecast.nn = function(fit, df, rep = 1, h = 1) {
  frc = c()
  na = data.frame(NA)
  a = df[nrow(df),-ncol(df)]
  aa = cbind(na,a)
  colnames(aa) = as.vector(names(df))
  frc[1] = predict(fit, aa, n.ahead = 1, rep = rep)
  if (h > 1) {
    for (i in 2:h) {
      aa[1,1] = frc[(i-1)]
      abaru = aa[nrow(aa),-(ncol(aa)-1)]
      aa = cbind(na,abaru)
      colnames(aa) = as.vector(names(df))
      frc[i] = predict(fit, aa, n.ahead = 1, rep = rep)
    }
  }
  return(frc)
}

#function k cross validation on rolling forecasting origin, stepmax=100000
ts.kcv = function(Trainset, Testset, lr, hn){
  ntrain = nrow(Trainset)
  ntest = nrow(Testset)
  frc.h = c()
  for (i in 1:ntest) {
    if (i == 1) {
      nnCovid <- neuralnet(Ytarget ~ ., data = Trainset, hidden = hn,
                           linear.output = F, algorithm = 'backprop',
                           learningrate = lr, err.fct = 'sse',
                           act.fct = 'logistic', startweights = NULL,
                           stepmax = 100000)
      frc.h[i] = forecast.nn(nnCovid, Trainset, h=1)
    }
    if (i > 1) {
      Trainset[(ntrain+i-1),] = Testset[(i-1),]
      nnCovid <- neuralnet(Ytarget ~ ., data = Trainset, hidden = hn,
                           linear.output = F, algorithm = 'backprop',
                           learningrate = lr, err.fct = 'sse',
                           act.fct = 'logistic', startweights = NULL,
                           stepmax = 100000)
      frc.h[i] = forecast.nn(nnCovid, Trainset, h=1)
    }
  }
  e.h = frc.h - Testset$Ytarget
  mse.h = mean(e.h^2)
  return(list(frc.h=frc.h, e.h=e.h, mse.h=mse.h))
}

hasilnn27 = data.frame(hidden,mse.jkt27); hasilnn27
hasilnn27[which(hasilnn27$mse==min(hasilnn27$mse.jkt27)),] # best hidden = 28
y.test = as.ts(denormalize(test$Ytarget, data = jktData));
frc.test = as.ts(denormalize(readRDS("jktnn27h28.rds")$frc, data = jktData))

# plot actual vs predicted hidden layer
ts.plot(y.test,frc.test,lty=c(1,3),col=c(4,10),
        main="Plot Hasil Ramalan Data Training Terhadap Data Testing")
legend("topright",c("aktual", "prediksi"), bty = "n",
       lty=c(1,3), col=c(4,10),lwd=c(1,1), cex = 0.8)

lr2 = readRDS("jkth28lr2.rds") ; lr6 = readRDS("jkth28lr6.rds")
lr7 = readRDS("jkth28lr7.rds") ; lr8 = readRDS("jkth28lr8.rds") 
lr9 = readRDS("jkth28lr9.rds") ; lr10 = readRDS("jktnn27h28.rds") 
lr11 = readRDS("jkth28lr11.rds") ;lr12 = readRDS("jkth28lr12.rds")
lr13 = readRDS("jkth28lr13.rds") ;lr14 = readRDS("jkth28lr14.rds") 
lr15 = readRDS("jkth28lr15.rds") ;lr16 = readRDS("jkth28lr16.rds") 
lr17 = readRDS("jkth28lr17.rds") ;lr18 = readRDS("jkth28lr18.rds") 
lr19 = readRDS("jkth28lr19.rds") ;lr20 = readRDS("jkth28lr20.rds") 

listlr = list(lr2,lr6,lr7,lr8,lr9,lr10,lr11,lr12,lr13,lr14,lr15,lr16,lr17,lr18,lr19,lr20)

lr = c()
mse.nn28lr = c()
for (i in 1:16) {
  if(i == 1){
    lr[i] = 0.002
    mse.nn28lr[i] = listlr[[i]]$mse
  }
  if(i == 2) {
    lr[i] = 0.006
    mse.nn28lr[i] = listlr[[i]]$mse
  }
  for(i in 3:16) {
    lr[i] = 0.006 + 0.001*(i-2)
    mse.nn28lr[i] = listlr[[i]]$mse
  }
}


lrtrial = data.frame(lr, mse.nn28lr);lrtrial
lrtrial[which(lrtrial$mse==min(lrtrial$mse.nn28lr)),]

y.test = as.ts(denormalize(test$Ytarget, data = jktData));
frc.test = as.ts(denormalize(readRDS("jktnn27h28.rds")$frc, data = jktData))
ts.plot(y.test,frc.test,lty=c(1,3),col=c(4,10),
        main="Plot Hasil Ramalan Data Training Terhadap Data Testing")
legend("topright",c("aktual", "prediksi"), bty = "n",
       lty=c(1,3), col=c(4,10),lwd=c(1,1), cex = 0.8)

#memodelkan full data dari model nn terbaik, best hidden = 28, best learning rate = 0.01
#saveRDS(neuralnet(Ytarget ~ ., data = trans, hidden = 28,
#                  linear.output = F, algorithm = 'backprop',
#                  learningrate = 0.01, err.fct = 'sse', rep = 20,
#                  act.fct = 'logistic', startweights = NULL,
#                  stepmax = 100000), file = "nnjkt.rds")


nnCovid = readRDS("nnjkt.rds")
input = subset(trans, select = -Ytarget)
rep = c()
mse = c()
Rsq = c()
AdjRsq = c()
for (i in 1:20) {
  comp.nnCovid = compute(nnCovid, input, rep = i)
  y = as.ts(denormalize(trans$Ytarget, data = jktData))
  yhat = as.ts(denormalize(comp.nnCovid$net.result, data=jktData))
  T = nrow(jktData)
  k = ncol(jktData) - 1
  rep[i] = i
  mse[i] = mean((y-yhat)^2) #ini mse model fix
  Rsq[i] = sum((yhat-mean(y))^2)/sum((y-mean(y))^2)
  AdjRsq[i] = 1-(((1-Rsq[i])*(T-1))/(T-k-1))
}
hasil = data.frame(rep=rep, mse=mse, Rsq=Rsq, AdjRsq=AdjRsq);hasil
comp.nnCovid = compute(nnCovid, input, rep = 13)
#########
y = as.ts(denormalize(trans$Ytarget, data = jktData))
yhat = as.ts(denormalize(comp.nnCovid$net.result, data = jktData))
e = ts(y-yhat)
library(FitAR)
LjungBoxTest(e, SquaredQ = F)

# plot actual vs predicted
ydat = data.frame(y,yhat)
colors = c("actual" = "blue", "predicted" = "red")
ggplot(ydat, aes(x = seq.int(from = 1, to = 737, by = 1))) +
  geom_line(aes(y=y, color = "actual"),size = 0.2) +
  geom_line(aes(y=yhat,  color = "predicted"), size = 0.2, linetype = "dashed") +
  scale_y_continuous(labels = function(y) format(y, big.mark = ",",
                                                 scientific = FALSE)) +
  labs(x = NULL, y = NULL) +
  scale_colour_manual(values = colors) +
  ggtitle("Predicted of Daily Positive Cases Covid 19 Jakarta")
  
nn.for = forecast.nn(nnCovid,trans, h = 7, rep = 10)
forecast = ts(denormalize(nn.for, data =covidJKT), start = c(2022, as.numeric(format(datefor[1],"%j"))),
              frequency = 365) #end nya janlup diubah
y = ts(denormalize(trans$Ytarget, data = jktData), start = c(2020, as.numeric(format(datejkt[28],"%j"))),
       frequency = 365)
autoplot(y) + autolayer(forecast) +
  scale_y_continuous(labels = function(y) format(y, big.mark = ",",
                                                 scientific = FALSE)) +
  ggtitle("Daily Positive Cases Covid 19 Jakarta")

datefor = c(seq(as.Date("2022-04-04"), as.Date("2022-04-10"), by = "day"))
df = data.frame(datefor, c(forecast))
plot(forecast~factor(datefor), c(forecast), type = "l", xlab = "Day", ylab = "Positive Cases", main = "Positive Cases")
library(ggplot2)
ggplot(df, aes(x=datefor, y=c(forecast))) + 
  labs(x="Date", y="Forecast of Daily Positive Cases") + 
  geom_line() +
  scale_y_continuous(labels = function(y) format(y, big.mark = ",",
                                                 scientific = FALSE))
# R Squared model fix
RSQUARE = function(y_actual,y_predict){
  sum((y_predict-mean(y_actual))^2)/sum((y_actual-mean(y_actual))^2)
}
RSQUARE(y,yhat)

# Ukuran kesalahan model fix
MAE(yhat,y)
RMSE(yhat,y)
