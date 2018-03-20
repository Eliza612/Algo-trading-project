#This r file is used to do backtest and plot graph. We can set parameters of predition period, lag and weight in line 521, 522 and 523. We can reset the parameter of "contract" to change the commodities that we test. Some parts come from our cited article.  We can also use parameter "A" and "B" change linear model.
#If we want to use basic model, just make strategy="A", and if we want to use improved mode let strategy="B"
#We can use for loop to test different parameters in order to find the parmeter that can generate highest profit.
path = setwd("C:/Users/HL/Desktop/Courses/Algo_Trading/Final/run3")
print(path)
## set contract we are going to backtest
contract = 'ni1705'
temp = data.frame(read.csv(paste(path,'contracti.csv',sep='/'),header=F))
colnames(temp) = c('contract','threshold','dot')
dot = temp[which(temp$contract==substr(contract,1,2)),3]
threshold = temp[which(temp$contract==substr(contract,1,2)),2]
reader = function(path) {
  files = list.files(path = path)
  data = list()
  for (f in files) {
    temp = read.csv(paste(path,f,sep='/'), header=T, stringsAsFactors=F)
    key = substr(f,2,9)
    
    a1=as.numeric(substr(temp$thetime,1,2))*3600
    a2=as.numeric(substr(temp$thetime,4,5))*60
    a3=as.numeric(substr(temp$thetime,7,8))
    temp['second_of_day'] =a1 +a2+a3
    contract = future(temp, open.int=F)
    
    data[[key]] = temp[which(temp$instrumentid == contract),]
    
    cat(nrow(data[[key]]),"\n")
    
    
  }
  return(data)
}
mystrategy = function(data,contract,threshold,dot,period,lags,strategy,weight){
  
  pnl.name = paste(contract,threshold,dot,period,lags,strategy,weight,sep='_')
  assign(pnl.name,matrix(nrow=length(data),ncol=4))
  pnl.matrix = get(pnl.name)
  pnl.matrix[1,] = 0
  
  
  coefs = {}
  for(i in 1:length(data)){
    key = names(data)[i]
    
    value = builder(data[[key]],full.day=T,delay=period,lags=lags,strategy=strategy,dot=dot)
    model = value$model
    coefs = rbind(coefs,model$coefficients)
  }
  
  
  dw = rep(1/weight,weight)
  coef.weights = dw
  
  
  trade.pnl = c()
  day.pnl = c()
  cum.pnl = c()
  trade.volume = 0
  trade.costs = 0
  
  
  
  for (i in 1:length(data)) {
    key = names(data)[i]
    if (i > 1) {
      coef = 0 
      w = coef.weights[1:min(length(coef.weights),i-1)]
      w = w/sum(w)
      for (j in 1:length(w)){
        coef = coef + coefs[i-j,] * w[j]
      }
      
      
      strat = runstrategy(data[[key]],coefs=coef,lags=lags,strategy=strategy,morning=F,time.flag=T,threshold=threshold,dot=dot)
      pnl.matrix[i,1] = strat$pnl
      trade.pnl = c(trade.pnl,strat$trade.pnl) 
      tv1 = strat$trade.volume
      tc1 = strat$trade.costs
      
      strat = runstrategy(data[[key]],coefs=coef,lags=lags,strategy=strategy,morning=F,time.flag=F,threshold=threshold,dot=dot)
      if (is.null(strat)) {
        pnl.matrix[i,2] = 0
        tv2 = 0
        tc2 = 0
      } else {
        pnl.matrix[i,2] = strat$pnl
        trade.pnl = c(trade.pnl,strat$trade.pnl)
        tv2 = strat$trade.volume
        tc2 = strat$trade.costs
      }
      
      strat = runstrategy(data[[key]],coefs=coef,lags=lags,strategy=strategy,morning=T,time.flag=T,threshold=threshold,dot=dot)
      pnl.matrix[i,3] = strat$pnl
      trade.pnl = c(trade.pnl,strat$trade.pnl)
      tv3 = strat$trade.volume
      tc3 = strat$trade.costs
      
      strat = runstrategy(data[[key]],coefs=coef,lags=lags,strategy=strategy,morning=T,time.flag=F,threshold=threshold,dot=dot)
      pnl.matrix[i,4] = strat$pnl
      trade.pnl = c(trade.pnl,strat$trade.pnl)
      tv = tv1+tv2+tv3+strat$trade.volume
      trade.volume = c(trade.volume,tv)
      tc = tc1+tc2+tc3+strat$trade.costs
      trade.costs = c(trade.costs,tc)
    }
    print(paste(key,threshold,dot,period,lags,strategy,weight,'DAY_PNL=',sum(pnl.matrix[i,]),'CUM_PNL=',sum(pnl.matrix[1:i,])))
    day.pnl = c(day.pnl,sum(pnl.matrix[i,]))
    cum.pnl = c(cum.pnl,sum(pnl.matrix[1:i,]))
  }  
  write.csv(trade.pnl,paste(getwd(),'trade_pnl.csv',sep='/'))
  
  
  value = {}
  value = cbind(pnl.matrix,day.pnl,cum.pnl,trade.volume,trade.costs)
  value = data.frame(value)
  rownames(value) = {}
  colnames(value) = c('e0','e1','m0','m1','dp','cp','tv','tc')
  return(value)	
}
builder = function(data, full.day = T, open.int = F,morning = F, time.flag = F, delay = 5, lags = 5, strategy = 'B', functions = NULL,dot) {
	morning.data = generate(data, morning = T, time.flag = T, open.int = open.int, delay = delay, lags = lags, functions = functions,dot=dot)
	morning1.data = generate(data, morning = T, time.flag = F, open.int = open.int, delay = delay, lags = lags, functions = functions,dot=dot)
	evening.data = generate(data, morning = F, time.flag=T, open.int = open.int, delay = delay, lags = lags, functions = functions,dot=dot)
	evening1.data = generate(data, morning = F, time.flag=F, open.int = open.int, delay = delay, lags = lags, functions = functions,dot=dot)
	dMid.Response = c(evening.data$dMid.Response,evening1.data$dMid.Response,morning.data$dMid.Response,morning1.data$dMid.Response)
	VOI = rbind(evening.data$VOI,evening1.data$VOI,morning.data$VOI,morning1.data$VOI)
	OIR = rbind(evening.data$OIR,evening1.data$OIR,morning.data$OIR,morning1.data$OIR)
	MPB = rbind(evening.data$MPB,evening1.data$MPB,morning.data$MPB,morning1.data$MPB)
	time.secs = c(evening.data$time.secs,evening1.data$time.secs,morning.data$time.secs,morning1.data$time.secs)
	mid.price = c(evening.data$mid.price,evening1.data$mid.price,morning.data$mid.price,morning1.data$mid.price)
	spread = c(evening.data$spread,evening1.data$spread,morning.data$spread,morning1.data$spread)
	AvgTrade.price = c(evening.data$AvgTrade.price,evening1.data$AvgTrade.price,morning.data$AvgTrade.price,morning1.data$AvgTrade.price)
	trading.data = rbind(evening.data$data,evening1.data$data,morning.data$data,morning1.data$data)

	spread = na.locf(na.locf(spread),fromLast=T)
	identity = function(x) x
	inverse = function(x) 1/x
	f.VOI = if(is.null(functions[['VOI']])) identity else functions[['VOI']] 
	f.OIR = if(is.null(functions[['OIR']])) identity else functions[['OIR']]
	
	
	x = list()
	x[['A']] = data.frame(y = dMid.Response, VOI = VOI)
	x[['B']] = data.frame(y = dMid.Response, VOI = VOI/spread, OIR = OIR/spread, MPB = MPB[,1]/spread)
	
	
	x[['B']][sapply(x[['B']],is.infinite)] = NA
	na.locf(na.locf(x[['B']],na.rm=F),fromLast=T)
	
	
	
	value = {}
	
	if (strategy != ' ') {
		s = strategy
		value$model = lm(y ~ ., data = x[[strategy]])
		tstats = coef(value$model)/sqrt(diag(vcov(value$model))) 
		print(coef(value$model))
		
	}
	
	value$dMid.Response = dMid.Response 
	value$VOI = VOI
	value$OIR = OIR
	value$MPB = MPB
	value$spread = spread
	value$y = dMid.Response
	value$x = x
	value$data = trading.data
	value$AvgTrade.price = AvgTrade.price
	value$mid.price = mid.price
	value$time.secs = time.secs
	value$tstats = tstats
	
	return(value)
}
generate = function(data,morning=F,time.flag=F,open.int=F,delay=20,lags=5,functions=NULL,dot){
  library(zoo)
  library(TTR)

  day.start = 0 
  
  AM1.start = 75600
  AM1.open = 75720
  AM1.close = 86280
  AM1.end = 86399
  
  PM1.start = 0
  PM1.open = 120
  PM1.close = 3480
  PM1.end = 3600
  
  AM.start = 32400 
  AM.open = 32520 
  AM.close = 41280 
  AM.end = 41400 
  
  PM.start = 48600 
  PM.open = 48720 
  PM.close = 53880 
  PM.end = 54000 
  
  
  
  start.time = ifelse(morning,ifelse(time.flag,AM.start,PM.start),ifelse(time.flag,AM1.start, PM1.start)) 
  open.time = ifelse(morning,ifelse(time.flag,AM.open,PM.open),ifelse(time.flag,AM1.open, PM1.open)) 
  close.time = ifelse(morning,ifelse(time.flag,AM.close,PM.close),ifelse(time.flag,AM1.close, PM1.close)) 
  end.time = ifelse(morning,ifelse(time.flag,AM.end,PM.end),ifelse(time.flag,AM1.end, PM1.end)) 
  

  instrument = future(data, open.int)
  
  ind = which(data$instrumentid=='ni1705' & data$second_of_day >= start.time & data$second_of_day < end.time) 

  main.data = data[ind,]
  n = nrow(main.data)
  if (n == 0){
    return({});
  }

  
  time.secs = main.data$second_of_day + main.data$themilltime/1000
  ind.open = head(which(time.secs>=open.time),1) 
  ind.close = tail(which(time.secs<=close.time),1)

  

  mid.price = (main.data$bid1_price + main.data$ask1_price)/2 
  mid.price[which(is.infinite(mid.price))] = NA
  mid.price = na.locf(na.locf(mid.price, na.rm=F), fromLast=T)
  spread = main.data$ask1_price-main.data$bid1_price
  
  OIR.array = (main.data$bid1_vol - main.data$ask1_vol) / (main.data$bid1_vol + main.data$ask1_vol)
  dBid.price = c(0,diff(main.data$bid1_price))
  dAsk.price = c(0,diff(main.data$ask1_price))
  

  bid.CV = (main.data$bid1_vol - ifelse(dBid.price==0,c(0,main.data$bid1_vol[1:(n-1)]),rep(0,n)))*as.integer(dBid.price>=0)
  ask.CV = (main.data$ask1_vol - ifelse(dAsk.price==0,c(0,main.data$ask1_vol[1:(n-1)]),rep(0,n)))*as.integer(dAsk.price<=0) 
  VOI.array = bid.CV - ask.CV
  
  dVol = c(NA,diff(main.data$volume))
  dTO = c(NA,diff(main.data$turnover))
  AvgTrade.price = dTO / dVol / dot
  AvgTrade.price[which(is.infinite(AvgTrade.price))] = NA
  AvgTrade.price = na.locf(na.locf(AvgTrade.price, na.rm=F), fromLast=T)
  MPB.array = AvgTrade.price - c(mid.price[1], rollmean(mid.price, k=2)) 
  
  k = delay
  p = lags
  new.ind = (p+1):(n-k)
  

  if (k > 0) {
    library(zoo)
    fpc = rollmean(mid.price, k=k)[-1] - mid.price[1:(n-k)]
    dMid.Response = c(fpc, rep(NA,k))
  } else {
    dMid.Response = rep(0,n)
  }

  VOI = cbind(VOI.array)
  OIR = cbind(OIR.array)
  MPB = cbind(MPB.array)
  if (p>0) {
    for (j in 1:p) {
      VOI = cbind(VOI, c(rep(NA,j), VOI.array[1:(n-j)]))
      OIR = cbind(OIR, c(rep(NA,j), OIR.array[1:(n-j)]))
      MPB = cbind(MPB, c(rep(NA,j), MPB.array[1:(n-j)])) 
    }
  }
  

  dMid.Response = dMid.Response[new.ind]
  VOI = VOI[new.ind,]
  OIR = OIR[new.ind,]
  MPB = MPB[new.ind,]

  colnames(VOI) = paste('t',seq(0,p),sep='')
  colnames(OIR) = paste('t',seq(0,p),sep='')
  colnames(MPB) = paste('t',seq(0,p),sep='')
  

  main.data = main.data[new.ind,]
  
  mid.price = mid.price[new.ind]
  spread = spread[new.ind]
  AvgTrade.price = AvgTrade.price[new.ind]
  
  time.secs = time.secs[new.ind]
  ind.open = ind.open-p
  ind.close = ind.close-p
  value = {}
  value$data = main.data
  value$dMid.Response = dMid.Response
  value$VOI = VOI
  value$OIR = OIR
  value$MPB = MPB
  
  value$time.secs = time.secs
  value$ind.open = ind.open
  value$ind.close = ind.close
  
  value$mid.price = mid.price
  value$spread = spread
  value$AvgTrade.price = AvgTrade.price
  
  return(value)
}
future = function(data, open.int = F) {
	if (open.int == T) {
		data = data[which(data$second_of_day < 75720 & data$second_of_day >= 75600),]
		data = data[data$open_interest==max(data$open_interest),]
	} else {
		data = data[which(data$second_of_day < 75720 & data$second_of_day >= 75600),]
		data = data[data$volume==max(data$volume),]
	}
	return(head(data$instrumentid,1))
}
runstrategy = function(data,coefs,lags,strategy='A', threshold=0.2, morning=F, time.flag=F, open.int=F, trade.at.mid=F, functions=NULL,dot) {
	
	TR.COST = 2.5*10^(-5)
	value = generate(data, morning=morning,time.flag=time.flag,open.int=open.int,delay=0,lags=lags,dot=dot)
	main.data = value$data
	if (is.null(main.data)) {
		return({})
	}
	n = nrow(main.data)

	
	spread = value$spread
	time.secs = value$time.secs
	mid.price = value$mid.price

	ind.open = value$ind.open
	ind.close = value$ind.close
	
	spread[which(spread==0)] = NA
	spread = na.locf(na.locf(spread,na.rm=F),fromLast=T)
	own = F
	pos = 0
	buy.price = 0
	sell.price = 0
	entry = 0
	trade.volume = 0
	tc = 0
	strat = rep(0,n)
	realized.pnl = rep(NA,n)
	total.trade.pnl = c()
	returns = c()
	pnl = 0
	trade.costs = 0
	sharpes = c()
	bid = if(trade.at.mid) mid.price*dot else main.data$bid1_price*dot
	ask = if(trade.at.mid) mid.price*dot else main.data$ask1_price*dot
	
	OIR = value$OIR
	MPB = value$MPB
	VOI = value$VOI

	
	OIR[sapply(OIR,is.infinite)] = NA
	MPB[sapply(MPB,is.infinite)] = NA
	OIR = na.locf(na.locf(OIR,na.rm=F),fromLast=T)
	MPB = na.locf(na.locf(MPB,na.rm=F),fromlast=T)
	
	identity = function(x) x
	f.OIR = if(is.null(functions[['OIR']])) identity else functions[['OIR']]
	
	f.VOI = if(is.null(functions[['VOI']])) identity else functions[['VOI']]

	x = cbind(rep(1,n))
	if (strategy == 'B') {
		
		x = cbind(x, f.VOI(VOI) / spread, f.OIR(OIR) / spread, MPB[,1] / spread)
		
	} else if (strategy == 'A') {
	  x = cbind(x, f.VOI(VOI))
	  
	  

	  	} else {
		
	  stop(paste('Missing Linear Strategy:', strategy))	
	
	  }

	efpc.vec = x %*% matrix(coefs, ncol = 1)
	efpc.vec[sapply(efpc.vec,is.infinite)] = 0
	efpc.vec = na.locf(na.locf(efpc.vec,na.rm=F),fromLast=T)
		
	k = 0
	for (t in time.secs) {
		
		k = k + 1
		efpc = efpc.vec[k]
		
		if(k >= ind.open & k < ind.close & own == F & efpc <= -threshold) {
			
		  strat[k] = -1
		  own = T
		  pos = -1
		  sell.price = bid[k]
		  entry = k
		  tc = sell.price * TR.COST
		  
		  trade.volume = trade.volume + 1
		  
		} else if (k >= ind.open & k < ind.close & own == F & efpc >= threshold) {
			
		  strat[k] = 1
		  own = T	
		  pos = 1
		  buy.price = ask[k]
		  entry = k
		  tc = buy.price * TR.COST
		  
		  trade.volume = trade.volume + 1
		  
		} else if (own == T & pos == 1 & efpc <= -threshold  ) {
			
			strat[k] = -1
			own = F
			pos = 0
			sell.price = bid[k]
			tc = tc + sell.price * TR.COST
			trade.costs = trade.costs + tc
			trade.pnl = sell.price - buy.price - tc
			pnl = pnl + trade.pnl
			trade.volume = trade.volume + 1
			total.trade.pnl = c(total.trade.pnl, trade.pnl)
			
			if (k >= ind.open & k < ind.close) {
				
				strat[k] = -2
				own = T
				pos = -1
				sell.price = bid[k]
				tc = sell.price * TR.COST
				
				trade.volume = trade.volume + 1
			}
		} else if (own == T & pos == -1 & efpc >= threshold) {
			
			strat[k] = 1
			own = F
			pos = 0
			buy.price = ask[k]
			tc = tc + buy.price * TR.COST
			trade.costs = trade.costs + tc
			trade.pnl = sell.price - buy.price - tc
			pnl = pnl + trade.pnl
			trade.volume = trade.volume + 1
			total.trade.pnl = c(total.trade.pnl, trade.pnl)

			if (k >= ind.open & k < ind.close) {
				
				strat[k] = 2
				own = T
				pos = 1
				buy.price = ask[k]
				entry = k
				tc = buy.price * TR.COST
				
				trade.volume = trade.volume + 1
			}
		}
		
		realized.pnl[k] = pnl
	}
	
	if (sum(strat) == 1) {
		if (strat[n] == 1) {
			strat[n] = 0
			trade.volume = trade.volume - 1
		} else {
			strat[n] = -1  
			sell.price = bid[n] 
			tc = tc + sell.price * TR.COST
			trade.costs = trade.costs + tc
			trade.pnl = sell.price - buy.price -tc
			pnl = pnl + trade.pnl
			
			total.trade.pnl = c(total.trade.pnl, trade.pnl)
			trade.volume = trade.volume + 1
		}
	} else if (sum(strat == -1)) {
		if (strat[n] == -1) {
			strat[n] = 0
			trade.volume = trade.volume - 1
		} else {
			strat[n] = 1
			buy.price = ask[n]
			tc = tc + buy.price * TR.COST
			trade.costs = trade.costs + tc
			trade.pnl = sell.price - buy.price - tc
			pnl = pnl + trade.pnl
			
			total.trade.pnl = c(total.trade.pnl, trade.pnl)
			trade.volume = trade.volume + 1
		}
	}
		value = {}
	value$time = time.secs
	value$pnl = pnl
	value$strategy = strat
	value$trade.volume = trade.volume
	value$trade.pnl = total.trade.pnl
	value$trade.costs = trade.costs
	print(paste(trade.volume,trade.costs,pnl))
	return(value)
	
}
## Loading the data
data = reader(paste(path,'dataset1',contract,sep='/'))
print(paste('backtest is going to start on',contract,threshold,dot))
## set variables to record
mesh = {}
## run backtest
##set trading and model parameters
for (ii in 5:100) {
  for (jj in 8:13) {
    for (kk in 1:1){
      ##set trading and model parameters
      period = ii
      lags = jj
      strategy = 'B'
      weight = kk
      ##run strategy
      backtest = mystrategy(data=data,contract=contract,threshold=threshold,dot=dot,period=period,lags=lags,strategy=strategy,weight=weight)
      print(backtest)
      total.pnl = c(ii,jj,backtest$cp[length(data)])
      mesh = rbind(mesh,total.pnl)
    }
  }
}
write.csv(backtest,paste(path,'ni.csv',sep='/'),row.names=F)
plot(c(1:23),backtest$cp,type="l", main="Rebar Cumulative PnL",xlab="Day",ylab="Value")
par(new=TRUE)
plot(c(1:23),backtest$tc,xaxt='n',yaxt='n',ann=FALSE,pch = 1)
axis(4, col.axis="black")
par(new=TRUE)
plot(c(1:23),backtest$tv,xaxt='n',yaxt='n',ann=FALSE,pch = 4,ylim= c(0,2000))
rownames(mesh) = {}
colnames(mesh) = c('K','L','P')#K prediction,L lag,P c profit
mesh = data.frame(mesh)
write.csv(backtest,paste(path,'klp_f.csv',sep='/'),row.names=F)
