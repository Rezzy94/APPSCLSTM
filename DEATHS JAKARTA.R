library(neuralnet)
library(forecast)
library(MLmetrics)
library(ggplot2)
# data
library("openxlsx")
datadeath = read.xlsx("C:/Users/ALAMANDA/Downloads/paper ADW/angka meninggal covid jkt.xlsx")
deathjkt=datadeath[,2];deathjkt
datejkt=seq(as.Date("2020-03-01"), as.Date("2022-04-03"), by = "day")
deathsJKT=ts(deathjkt, start = c(2020, as.numeric(format(datejkt[1],"%j"))),
             frequency = 365)
autoplot(deathsJKT)
pacf(deathsJKT, main = "Series Death Cases Caused by COVID-19 Jakarta")

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
jktData = makeinput(deathsJKT, lags = 24)

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
      nndeaths <- neuralnet(Ytarget ~ ., data = Trainset, hidden = hn,
                            linear.output = F, algorithm = 'backprop',
                            learningrate = lr, err.fct = 'sse',
                            act.fct = 'logistic', startweights = NULL,
                            stepmax = 100000)
      frc.h[i] = forecast.nn(nndeaths, Trainset, h=1)
    }
    if (i > 1) {
      Trainset[(ntrain+i-1),] = Testset[(i-1),]
      nndeaths <- neuralnet(Ytarget ~ ., data = Trainset, hidden = hn,
                            linear.output = F, algorithm = 'backprop',
                            learningrate = lr, err.fct = 'sse',
                            act.fct = 'logistic', startweights = NULL,
                            stepmax = 100000)
      frc.h[i] = forecast.nn(nndeaths, Trainset, h=1)
    }
  }
  e.h = frc.h - Testset$Ytarget
  mse.h = mean(e.h^2)
  return(list(frc.h=frc.h, e.h=e.h, mse.h=mse.h))
}

# lr = 0.01
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 5), file = "death_hn5.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 6), file = "death_hn6.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 7), file = "death_hn7.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 8), file = "death_hn8.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 9), file = "death_hn9.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 10), file = "death_hn10.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 11), file = "death_hn11.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 12), file = "death_hn12.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 13), file = "death_hn13.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 14), file = "death_hn14.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 15), file = "death_hn15.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 16), file = "death_hn16.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 17), file = "death_hn17.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 18), file = "death_hn18.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 19), file = "death_hn19.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 20), file = "death_hn20.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 21), file = "death_hn21.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 22), file = "death_hn22.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 23), file = "death_hn23.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 24), file = "death_hn24.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 25), file = "death_hn25.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 26), file = "death_hn26.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 27), file = "death_hn27.rds")
#saveRDS(ts.kcv(train, test, lr = 0.01, hn = 28), file = "death_hn28.rds")

h5 = readRDS("death_hn5.rds") ;h6 = readRDS("death_hn6.rds")
h7 = readRDS("death_hn7.rds") ;h8 = readRDS("death_hn8.rds")
h9 = readRDS("death_hn9.rds") ;h10 = readRDS("death_hn10.rds")
h11 = readRDS("death_hn11.rds") ;h12 = readRDS("death_hn12.rds") 
h13 = readRDS("death_hn13.rds") ;h14 = readRDS("death_hn14.rds") 
h15 = readRDS("death_hn15.rds") ;h16 = readRDS("death_hn16.rds")
h17 = readRDS("death_hn17.rds") ;h18 = readRDS("death_hn18.rds") 
h19 = readRDS("death_hn19.rds") ;h20 = readRDS("death_hn20.rds") 
h21 = readRDS("death_hn21.rds") ;h22 = readRDS("death_hn22.rds") 
h23 = readRDS("death_hn23.rds") ;h24 = readRDS("death_hn24.rds") 
h25 = readRDS("death_hn25.rds") ;h26 = readRDS("death_hn26.rds") 
h27 = readRDS("death_hn27.rds") ;h28 = readRDS("death_hn28.rds")

listnn = list(h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,
              h18,h19,h20,h21,h22,h23,h24,h25,h26,h27,h28)
hidden = c()
mse.hn = c()
for (i in 1:24) {
  hidden[i] = i+4
  mse.hn[i] = listnn[[i]]$mse
}

hasil.hn = data.frame(hidden,mse.hn); hasil.hn
hasil.hn[which(hasil.hn$mse==min(hasil.hn$mse.hn)),] # best hidden = 9
y.test = as.ts(denormalize(test$Ytarget, data = jktData));
frc.test = as.ts(denormalize(readRDS("death_hn9.rds")$frc, data = jktData))
ts.plot(y.test,frc.test,lty=c(1,3),col=c(4,10),
        main="Plot Hasil Ramalan Data Training Terhadap Data Testing")
legend("topright",c("aktual", "prediksi"), bty = "n",
       lty=c(1,3), col=c(4,10),lwd=c(1,1), cex = 0.8)

#saveRDS(ts.kcv(train, test, lr = 0.004, hn = 9), file = "death_lr4.rds")
#saveRDS(ts.kcv(train, test, lr = 0.005, hn = 9), file = "death_lr5.rds")
#saveRDS(ts.kcv(train, test, lr = 0.006, hn = 9), file = "death_lr6.rds")
#saveRDS(ts.kcv(train, test, lr = 0.007, hn = 9), file = "death_lr7.rds")
#saveRDS(ts.kcv(train, test, lr = 0.008, hn = 9), file = "death_lr8.rds")
#saveRDS(ts.kcv(train, test, lr = 0.009, hn = 9), file = "death_lr9.rds")
#saveRDS(ts.kcv(train, test, lr = 0.011, hn = 9), file = "death_lr11.rds")
#saveRDS(ts.kcv(train, test, lr = 0.012, hn = 9), file = "death_lr12.rds")
#saveRDS(ts.kcv(train, test, lr = 0.013, hn = 9), file = "death_lr13.rds")
#saveRDS(ts.kcv(train, test, lr = 0.014, hn = 9), file = "death_lr14.rds")
#saveRDS(ts.kcv(train, test, lr = 0.015, hn = 9), file = "death_lr15.rds")
#saveRDS(ts.kcv(train, test, lr = 0.016, hn = 9), file = "death_lr16.rds")
#saveRDS(ts.kcv(train, test, lr = 0.017, hn = 9), file = "death_lr17.rds")
#saveRDS(ts.kcv(train, test, lr = 0.018, hn = 9), file = "death_lr18.rds")
#saveRDS(ts.kcv(train, test, lr = 0.019, hn = 9), file = "death_lr19.rds")
#saveRDS(ts.kcv(train, test, lr = 0.02, hn = 9), file = "death_lr20.rds")

lr4 = readRDS("death_lr4.rds")
lr5 = readRDS("death_lr5.rds") ; lr6 = readRDS("death_lr6.rds")
lr7 = readRDS("death_lr7.rds") ; lr8 = readRDS("death_lr8.rds") 
lr9 = readRDS("death_lr9.rds") ; lr10 = readRDS("death_hn9.rds") 
lr11 = readRDS("death_lr11.rds") ;lr12 = readRDS("death_lr12.rds")
lr13 = readRDS("death_lr13.rds") ;lr14 = readRDS("death_lr14.rds") 
lr15 = readRDS("death_lr15.rds") ;lr16 = readRDS("death_lr16.rds") 
lr17 = readRDS("death_lr17.rds") ;lr18 = readRDS("death_lr18.rds") 
lr19 = readRDS("death_lr19.rds") ;lr20 = readRDS("death_lr20.rds") 


listlr = list(lr4,lr5,lr6,lr7,lr8,lr9,lr10,lr11,lr12,lr13,lr14,lr15,lr16,lr17,lr18,lr19,lr20)

lr = c()
mse.hn9lr = c()
for (i in 1:17) {
  if(i == 1){
    lr[i] = 0.004
    mse.hn9lr[i] = listlr[[i]]$mse
  }
  for(i in 2:17) {
    lr[i] = 0.004 + 0.001*(i-1)
    mse.hn9lr[i] = listlr[[i]]$mse
  }
}

lrtrial = data.frame(lr, mse.hn9lr);lrtrial
lrtrial[which(lrtrial$mse==min(lrtrial$mse.hn9lr)),]

y.test = as.ts(denormalize(test$Ytarget, data = jktData));
frc.test = as.ts(denormalize(readRDS("death_hn9.rds")$frc, data = jktData))
ts.plot(y.test,frc.test,lty=c(1,3),col=c(4,10),
        main="Plot Hasil Ramalan Data Training Terhadap Data Testing")
legend("topright",c("aktual", "prediksi"), bty = "n",
       lty=c(1,3), col=c(4,10),lwd=c(1,1), cex = 0.8)

#memodelkan full data dari model nn terbaik, best hidden = 9, best learning rate = 0.01
#saveRDS(neuralnet(Ytarget ~ ., data = trans, hidden = 9,
#                  linear.output = F, algorithm = 'backprop',
#                  learningrate = 0.01, err.fct = 'sse', rep = 20,
#                  act.fct = 'logistic', startweights = NULL,
#                  stepmax = 100000), file = "deathJKT.rds")

nndeaths = readRDS("deathJKT.rds")
input = subset(trans, select = -Ytarget)
rep = c()
mse = c()
Rsq = c()
AdjRsq = c()
for (i in 1:20) {
  comp.nndeaths = compute(nndeaths, input, rep = i)
  y = as.ts(denormalize(trans$Ytarget, data = jktData))
  yhat = as.ts(denormalize(comp.nndeaths$net.result, data=jktData))
  T = nrow(jktData)
  k = ncol(jktData) - 1
  rep[i] = i
  mse[i] = mean((y-yhat)^2) #ini mse model fix
  Rsq[i] = sum((yhat-mean(y))^2)/sum((y-mean(y))^2)
  AdjRsq[i] = 1-(((1-Rsq[i])*(T-1))/(T-k-1))
}
hasil = data.frame(rep=rep, mse=mse, Rsq=Rsq, AdjRsq=AdjRsq);hasil
comp.nndeaths = compute(nndeaths, input, rep = 2)
#########
y = as.ts(denormalize(trans$Ytarget, data = jktData))
yhat = as.ts(denormalize(comp.nndeaths$net.result, data = jktData))
e = ts(y-yhat)
library(FitAR)
LjungBoxTest(e, SquaredQ = F)
ydat = data.frame(y,yhat)
colors = c("actual" = "blue", "predicted" = "red")
ggplot(ydat, aes(x = seq.int(from = 1, to = 740, by = 1))) +
  geom_line(aes(y=y, color = "actual"),size = 0.2) +
  geom_line(aes(y=yhat,  color = "predicted"), size = 0.2, linetype = "dashed") +
  scale_y_continuous(labels = function(y) format(y, big.mark = ",",
                                                 scientific = FALSE)) +
  labs(x = NULL, y = NULL) +
  scale_colour_manual(values = colors) +
  ggtitle("Predicted of Daily Death Cases Caused by Covid 19 Jakarta")

nn.for = forecast.nn(nndeaths, trans, h = 7, rep = 10)

forecast = ts(denormalize(nn.for, data = deathsJKT), start = 738, end = 744) #end nya janlup diubah
plot(datefor, forecast, type = "l", xlab = "Date", ylab = "Death Cases")

datedeath=seq(as.Date("2020-03-01"), as.Date("2022-04-03"), by = "day")
deaths = ts(deathsJKT, start = c(2020, as.numeric(format(datedeath[1],"%j"))),
           frequency = 365)
datefor = seq(as.Date("2022-04-04"), as.Date("2022-04-10"), by = "day")
forecast = ts(forecast, start = c(2022, as.numeric(format(datefor[1],"%j"))),
              frequency = 365)
autoplot(deaths, main = "Death Cases Caused by COVID-19", ylab = "Death Cases")+autolayer(forecast)

# R Squared model fix
RSQUARE = function(y_actual,y_predict){
  sum((y_predict-mean(y_actual))^2)/sum((y_actual-mean(y_actual))^2)
}
RSQUARE(y,yhat)

# Ukuran kesalahan model fix
MAE(yhat,y)
RMSE(yhat,y)

autoplot(round(forecast))
