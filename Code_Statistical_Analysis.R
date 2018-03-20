#import data for Ni1705 2017/03/01
a = read.csv("/Users/Ryan/Desktop/algo trading/project/group8/dataset1/ni1705/d20170301.csv")
a['second_of_day'] <- as.numeric(substr(a$thetime,1,2))*3600+as.numeric(substr(a$thetime,4,5))*60+as.numeric(substr(a$thetime,7,8))

dot <- 1
k <-5
peroid = which(a$second_of_day >= 0 & a$second_of_day < 50000) 
main.data <- a[peroid,]
n <- nrow(main.data)
if (n == 0){
  return({})
}

mid.price <- (main.data$bid1_price + main.data$ask1_price)/2 
mid.price[which(is.infinite(mid.price))] <- NA
library(zoo)
mid.price <- na.locf(na.locf(mid.price, na.rm=F), fromLast=T) #last observation carried over
spread <- main.data$ask1_price-main.data$bid1_price
OIR.array <- (main.data$bid1_vol - main.data$ask1_vol) / (main.data$bid1_vol + main.data$ask1_vol)
dBid.price <- c(0,diff(main.data$bid1_price))
dAsk.price <- c(0,diff(main.data$ask1_price))
bid.CV <- (main.data$bid1_vol - ifelse(dBid.price==0,c(0,main.data$bid1_vol[1:(n-1)]),rep(0,n)))*as.integer(dBid.price>=0)
ask.CV <- (main.data$ask1_vol - ifelse(dAsk.price==0,c(0,main.data$ask1_vol[1:(n-1)]),rep(0,n)))*as.integer(dAsk.price<=0) 
VOI.array <- bid.CV - ask.CV
dVol <- c(NA,diff(main.data$volume))
dTO <- c(NA,diff(main.data$turnover))
AvgTrade.price <- dTO / dVol / dot
AvgTrade.price[which(is.infinite(AvgTrade.price))] <- NA
AvgTrade.price <- na.locf(na.locf(AvgTrade.price, na.rm=F), fromLast=T)
MPB.array <- AvgTrade.price - c(mid.price[1], rollmean(mid.price, k=2))
## arithmetic average of future k midprices minus current midprice
if (k > 0) {
  library(zoo)
  fpc <- rollmean(mid.price, k=k)[-1] - mid.price[1:(n-k)]
  dMid.Response <- c(fpc, rep(NA,k))
} else {
  dMid.Response <- rep(0,n)
}

#Calculate autocorelation
acf(VOI.array,lag.max = 15)
acf(OIR.array,lag.max = 15)

#Check Variance Ratio
variance_ratio <- integer(99)
for (i in c(2:100)){
  j <- i
  var_j <- var(MPB.array[(j+1):length(MPB.array)] - MPB.array[1:(length(MPB.array)-j)])
  var_1 <- var(MPB.array[2:length(MPB.array)] - MPB.array[1:length(MPB.array)])
  variance_ratio[i-1] <- var_j/(j*var_1)
}

plot(c(2:100),variance_ratio, type = 'l',xlab="Lag",ylab="Variance Ratio",main="Variance Ratio for MPB")


#Draw Charts
par(mfrow = c(2,2))
plot(peroid,VOI.array,type = "l",xlab = "seconds", ylab = "VOI" ,main = "VOI")
plot(peroid,MPB.array,type = "l",xlab = "seconds", ylab = "MPB" , main = "MPB")
plot(peroid,OIR.array,type = "l",xlab = "seconds", ylab = "OIR" , main = "OIR")
plot(peroid,dMid.Response,type = "l",xlab = "seconds", ylab = "dMid.Response" , main = "Average Middle Price,k=5")

#ADF & KPSS Test
library(tseries)
adf.test(VOI.array)
adf.test(MPB.array)
adf.test(OIR.array)
adf.test(na.omit(dMid.Response))

kpss.test(VOI.array)
kpss.test(MPB.array)
kpss.test(OIR.array)
kpss.test(na.omit(dMid.Response))
