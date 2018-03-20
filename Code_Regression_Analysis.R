
read_data = function(a,b){
  
  file = read.csv(paste('~/Desktop/Algo_project/dataset1',a,b,sep='/'))
  file['second_of_day'] = as.numeric(substr(file$thetime,1,2))*3600+as.numeric(substr(file$thetime,4,5))*60+as.numeric(substr(file$thetime,7,8))
  return(file)
}

################################ Create variables 
dat = function(data,morning,time_flag,open_int,delay,lags,dot){
  #install.packages("zoo")
  #install.packages("TTR")
  library(zoo)
  library(TTR)
  #trading times
  day_start = 0 # 0:00
  AM1_start = 75600#21:00
  AM1_open = 75720#21:02
  AM1_close = 86280#23:58
  AM1_end = 86399#23:59:59
  
  PM1_start = 0#00:00
  PM1_open = 120#00:02
  PM1_close = 3480#00:58
  PM1_end = 3600#1:00
  
  AM_start = 32400 # 9:00
  AM_open = 32520 # 9:02 - trade open
  AM_close = 41280 # 11:28 - trade close
  AM_end = 41400 # 11:30
  
  PM_start = 48600 # 13:30
  PM_open = 48720 # 13:32 - trade open
  PM_close = 53880 # 14:58 - trade close
  PM_end = 54000 # 15:00
  
  
  
  start_time = ifelse(morning,ifelse(time_flag,AM_start,PM_start),ifelse(time_flag,AM1_start, PM1_start)) 
  open_time = ifelse(morning,ifelse(time_flag,AM_open,PM_open),ifelse(time_flag,AM1_open, PM1_open)) 
  close_time = ifelse(morning,ifelse(time_flag,AM_close,PM_close),ifelse(time_flag,AM1_close, PM1_close))
  end_time = ifelse(morning,ifelse(time_flag,AM_end,PM_end),ifelse(time_flag,AM1_end, PM1_end)) 
  
  instrument = head(data$instrumentid,1)
  ind = which(data$instrumentid==instrument & data$second_of_day >= start_time & data$second_of_day < end_time) 
  
  main_data = data[ind,]
  n = nrow(main_data)

  
  time_secs = main_data$second_of_day + main_data$themilltime/1000
  ind_open = head(which(time_secs>=open_time),1) 
  ind_close = tail(which(time_secs<=close_time),1)
 
  
  #calculate variables
  mid_price = (main_data$bid1_price + main_data$ask1_price)/2 
  mid_price[which(is.infinite(mid_price))] = NA
  mid_price = na.locf(na.locf(mid_price, na.rm=F), fromLast=T)
  spread = main_data$ask1_price-main_data$bid1_price
  
  OIR_array = (main_data$bid1_vol - main_data$ask1_vol) / (main_data$bid1_vol + main_data$ask1_vol)
  dBid_price = c(0,diff(main_data$bid1_price))
  dAsk_price = c(0,diff(main_data$ask1_price))
  
  ##build order imbalance signal according to Spec
  bid_CV = (main_data$bid1_vol - ifelse(dBid_price==0,c(0,main_data$bid1_vol[1:(n-1)]),rep(0,n)))*as.integer(dBid_price>=0)
  ask_CV = (main_data$ask1_vol - ifelse(dAsk_price==0,c(0,main_data$ask1_vol[1:(n-1)]),rep(0,n)))*as.integer(dAsk_price<=0) 
  VOI_array = bid_CV - ask_CV
  
  dVol = c(NA,diff(main_data$volume))
  dTO = c(NA,diff(main_data$turnover))
  AvgTrade_price = dTO / dVol / dot
  AvgTrade_price[which(is.infinite(AvgTrade_price))] = NA
  AvgTrade_price = na.locf(na.locf(AvgTrade_price, na.rm=F), fromLast=T)
  MPB_array = AvgTrade_price - c(mid_price[1], rollmean(mid_price, k=2)) 
  
 
  new_ind = (lags+1):(n-delay)
  
  ## arithmetic average of future k midprices minus current midprice
    fpc = rollmean(mid_price, k=delay)[-1] - mid_price[1:(n-delay)]
    dMid_Response = c(fpc, rep(NA,delay))

  
  #build VOI, dMid, OIR - first p entries are useless
  VOI = cbind(VOI_array)
  OIR = cbind(OIR_array)
  MPB = cbind(MPB_array)
 
    for (j in 1:lags) {
      VOI = cbind(VOI, c(rep(NA,j), VOI_array[1:(n-j)]))
      OIR = cbind(OIR, c(rep(NA,j), OIR_array[1:(n-j)]))
      MPB = cbind(MPB, c(rep(NA,j), MPB_array[1:(n-j)])) 
    }

  
  ##trim the variables
  dMid_Response = dMid_Response[new_ind]
  VOI = VOI[new_ind,]
  OIR = OIR[new_ind,]
  MPB = MPB[new_ind,]
  
  
  colnames(VOI) = c("VOI_to","VOI_t1","VOI_t2","VOI_t3","VOI_t4","VOI_t5") 
  colnames(OIR) = c("OIR_to","OIR_t1","OIR_t2","OIR_t3","OIR_t4","OIR_t5")
  colnames(MPB) = c("MPB_to","MPB_t1","MPB_t2","MPB_t3","MPB_t4","MPB_t5")
  
  ##trim the other supporting data
  main_data = main_data[new_ind,]
  
  mid_price = mid_price[new_ind]
  spread = spread[new_ind]
  AvgTrade_price = AvgTrade_price[new_ind]
  
  time_secs = time_secs[new_ind]
  ind_open = ind_open-lags
  ind_close = ind_close-lags
  
  ##return an R project
  a = list()
  a[["data"]] = main_data
  
  a[["dMid_Response"]] = dMid_Response
  a[["VOI"]] = VOI
  a[["OIR"]] = OIR
  a[["MPB"]] = MPB
  
  a[["time_secs"]] = time_secs
  a[["ind_open"]] = ind_open
  a[["ind_close"]] = ind_close
  
  a[["mid_price"]] = mid_price
  a[["spread"]] = spread
  a[["AvgTrade_price"]] = AvgTrade_price
  
  return(a)
}


##################################################### Create Model
model = function(data, full_day, open_int,morning, time_flag, delay, lags, strategy,dot) {
 
  
    morning_data = dat(data, T,  T,  F, delay, lags, dot=dot)
    morning1_data = dat(data, T,  F,  F, delay, lags,dot=dot)
    evening_data = dat(data, F, T,  F, delay, lags, dot=dot)
    evening1_data = dat(data,F, F,  F, delay, lags,dot=dot)
    dMid_Response = c(evening_data$dMid_Response,evening1_data$dMid_Response,morning_data$dMid_Response,morning1_data$dMid_Response)
    VOI = rbind(evening_data$VOI,evening1_data$VOI,morning_data$VOI,morning1_data$VOI)
    OIR = rbind(evening_data$OIR,evening1_data$OIR,morning_data$OIR,morning1_data$OIR)
    MPB = rbind(evening_data$MPB,evening1_data$MPB,morning_data$MPB,morning1_data$MPB)
    time_secs = c(evening_data$time_secs,evening1_data$time_secs,morning_data$time_secs,morning1_data$time_secs)
    mid_price = c(evening_data$mid_price,evening1_data$mid_price,morning_data$mid_price,morning1_data$mid_price)
    spread = c(evening_data$spread,evening1_data$spread,morning_data$spread,morning1_data$spread)
    AvgTrade_price = c(evening_data$AvgTrade_price,evening1_data$AvgTrade_price,morning_data$AvgTrade_price,morning1_data$AvgTrade_price)
    trading_data = rbind(evening_data$data,evening1_data$data,morning_data$data,morning1_data$data)
 
  
  ## Build data for linear regression
  variables= {}
  variables$A = data.frame(response = dMid_Response, VOI = VOI)
  variables$B= data.frame(response = dMid_Response, VOI = VOI/spread, OIR = OIR/spread, MPB = MPB[,1]/spread)
  

  
  #Build the linear medel using Linear Regression
    lm_model = lm(response~ ., data = variables[[strategy]])
    tstats = coef(lm_model)/sqrt(diag(vcov(lm_model))) # t-value
    coef = (coef(lm_model))
    p_value = (2*pt(abs(tstats),df = df.residual(lm_model),lower.tail=FALSE)) # p-value


    aaa = cbind(tstats,coef,p_value)
    colnames(aaa) = c("t stat","coefficient", "p_value")
    return(aaa)
}


#data for ni1705 in 2017/03/01
ni_day1 = read.csv('~/Desktop/Algo_project/dataset1/ni1705/d20170301.csv')
ni_day1['second_of_day'] <- as.numeric(substr(a$thetime,1,2))*3600+as.numeric(substr(a$thetime,4,5))*60+as.numeric(substr(a$thetime,7,8))

#basic model using ni1705 in 2017/03/01
model(ni_day1, T, F, F, F, 5, 5,'A',1)
#imporve model using ni1705 in 2017/03/01
model(ni_day1, T, F, F, F, 5,5,'B',1)







## Calculate percentage of variables when their coefficient are positive and singificant
## for basic mode and improved model
basic_output = function(a,b,dot){
  
  return(model(read_data(a,b), T, F,F,  F,  5, 5,  'A',dot))
  
}
improved_output = function(a,b,dot){
  
  return(model(read_data(a,b), T, F,F,  F,  5, 5,  'B',dot))
  
}

ni_out = rbind(basic_output('ni1705','d20170301.csv',1),basic_output('ni1705','d20170302.csv',1),basic_output('ni1705','d20170303.csv',1),basic_output('ni1705','d20170306.csv',1),
               basic_output('ni1705','d20170307.csv',1),basic_output('ni1705','d20170308.csv',1),basic_output('ni1705','d20170309.csv',1),basic_output('ni1705','d20170310.csv',1),
               basic_output('ni1705','d20170313.csv',1),basic_output('ni1705','d20170314.csv',1),basic_output('ni1705','d20170315.csv',1),basic_output('ni1705','d20170316.csv',1),
               basic_output('ni1705','d20170317.csv',1),basic_output('ni1705','d20170320.csv',1),basic_output('ni1705','d20170321.csv',1),basic_output('ni1705','d20170322.csv',1),
               basic_output('ni1705','d20170323.csv',1),basic_output('ni1705','d20170324.csv',1),basic_output('ni1705','d20170327.csv',1),basic_output('ni1705','d20170328.csv',1),
               basic_output('ni1705','d20170329.csv',1),basic_output('ni1705','d20170330.csv',1),basic_output('ni1705','d20170331.csv',1))
ni_out = as.data.frame(ni_out,row.names = FALSE)
ni_out$pos_sig = ifelse(ni_out$coefficient>0 &ni_out$p_value<0.05,1,0)

al_out = rbind(basic_output('al1705','d20170301.csv',5),basic_output('al1705','d20170302.csv',5),basic_output('al1705','d20170303.csv',5),basic_output('al1705','d20170306.csv',5),
               basic_output('al1705','d20170307.csv',5),basic_output('al1705','d20170308.csv',5),basic_output('al1705','d20170309.csv',5),basic_output('al1705','d20170310.csv',5),
               basic_output('al1705','d20170313.csv',5),basic_output('al1705','d20170314.csv',5),basic_output('al1705','d20170315.csv',5),basic_output('al1705','d20170316.csv',5),
               basic_output('al1705','d20170317.csv',5),basic_output('al1705','d20170320.csv',5),basic_output('al1705','d20170321.csv',5),basic_output('al1705','d20170322.csv',5),
               basic_output('al1705','d20170323.csv',5),basic_output('al1705','d20170324.csv',5),basic_output('al1705','d20170327.csv',5),basic_output('al1705','d20170328.csv',5),
               basic_output('al1705','d20170329.csv',5),basic_output('al1705','d20170330.csv',5),basic_output('al1705','d20170331.csv',5))
al_out = as.data.frame(al_out,row.names = FALSE)
al_out$pos_sig = ifelse(al_out$coefficient>0 & al_out$p_value<0.05,1,0)

cu_out = rbind(basic_output('cu1705','d20170301.csv',5),basic_output('cu1705','d20170302.csv',5),basic_output('cu1705','d20170303.csv',5),basic_output('cu1705','d20170306.csv',5),
               basic_output('cu1705','d20170307.csv',5),basic_output('cu1705','d20170308.csv',5),basic_output('cu1705','d20170309.csv',5),basic_output('cu1705','d20170310.csv',5),
               basic_output('cu1705','d20170313.csv',5),basic_output('cu1705','d20170314.csv',5),basic_output('cu1705','d20170315.csv',5),basic_output('cu1705','d20170316.csv',5),
               basic_output('cu1705','d20170317.csv',5),basic_output('cu1705','d20170320.csv',5),basic_output('cu1705','d20170321.csv',5),basic_output('cu1705','d20170322.csv',5),
               basic_output('cu1705','d20170323.csv',5),basic_output('cu1705','d20170324.csv',5),basic_output('cu1705','d20170327.csv',5),basic_output('cu1705','d20170328.csv',5),
               basic_output('cu1705','d20170329.csv',5),basic_output('cu1705','d20170330.csv',5),basic_output('cu1705','d20170331.csv',5))
cu_out = as.data.frame(cu_out,row.names = FALSE)
cu_out$pos_sig = ifelse(cu_out$coefficient>0 & cu_out$p_value<0.05,1,0)


ru_out = rbind(basic_output('ru1705','d20170301.csv',10),basic_output('ru1705','d20170302.csv',10),basic_output('ru1705','d20170303.csv',10),basic_output('ru1705','d20170306.csv',10),
               basic_output('ru1705','d20170307.csv',10),basic_output('ru1705','d20170308.csv',10),basic_output('ru1705','d20170309.csv',10),basic_output('ru1705','d20170310.csv',10),
               basic_output('ru1705','d20170313.csv',10),basic_output('ru1705','d20170314.csv',10),basic_output('ru1705','d20170315.csv',10),basic_output('ru1705','d20170316.csv',10),
               basic_output('ru1705','d20170317.csv',10),basic_output('ru1705','d20170320.csv',10),basic_output('ru1705','d20170321.csv',10),basic_output('ru1705','d20170322.csv',10),
               basic_output('ru1705','d20170323.csv',10),basic_output('ru1705','d20170324.csv',10),basic_output('ru1705','d20170327.csv',10),basic_output('ru1705','d20170328.csv',10),
               basic_output('ru1705','d20170329.csv',10),basic_output('ru1705','d20170330.csv',10),basic_output('ru1705','d20170331.csv',10))
ru_out = as.data.frame(ru_out,row.names = FALSE)
ru_out$pos_sig = ifelse(ru_out$coefficient>0 & ru_out$p_value<0.05,1,0)

rb_out = rbind(basic_output('rb1705','d20170301.csv',10),basic_output('rb1705','d20170302.csv',10),basic_output('rb1705','d20170303.csv',10),basic_output('rb1705','d20170306.csv',10),
               basic_output('rb1705','d20170307.csv',10),basic_output('rb1705','d20170308.csv',10),basic_output('rb1705','d20170309.csv',10),basic_output('rb1705','d20170310.csv',10),
               basic_output('rb1705','d20170313.csv',10),basic_output('rb1705','d20170314.csv',10),basic_output('rb1705','d20170315.csv',10),basic_output('rb1705','d20170316.csv',10),
               basic_output('rb1705','d20170317.csv',10),basic_output('rb1705','d20170320.csv',10),basic_output('rb1705','d20170321.csv',10),basic_output('rb1705','d20170322.csv',10),
               basic_output('rb1705','d20170323.csv',10),basic_output('rb1705','d20170324.csv',10),basic_output('rb1705','d20170327.csv',10),basic_output('rb1705','d20170328.csv',10),
               basic_output('rb1705','d20170329.csv',10),basic_output('rb1705','d20170330.csv',10),basic_output('rb1705','d20170331.csv',10))
rb_out = as.data.frame(rb_out,row.names = FALSE)
rb_out$pos_sig = ifelse(rb_out$coefficient>0 & rb_out$p_value<0.05,1,0)


#Calculate percentage of variables when their coefficient are positive and singificant
#Use basic model
basic_perc = function(dataset){
  intercept = dataset$pos_sig[seq(1,length(dataset$pos_sig),7)]
  VOI_t0 = dataset$pos_sig[seq(2,length(dataset$pos_sig),7)]
  VOI_t1 = dataset$pos_sig[seq(3,length(dataset$pos_sig),7)]
  VOI_t2 = dataset$pos_sig[seq(4,length(dataset$pos_sig),7)]
  VOI_t3 = dataset$pos_sig[seq(5,length(dataset$pos_sig),7)]
  VOI_t4 = dataset$pos_sig[seq(6,length(dataset$pos_sig),7)]
  VOI_t5 = dataset$pos_sig[seq(7,length(dataset$pos_sig),7)]
  intercept_perc = sum(intercept)/length(intercept)
  VOI_t0_perc = sum(VOI_t0)/length(VOI_t0)*100
  VOI_t1_perc = sum(VOI_t1)/length(VOI_t1)*100
  VOI_t2_perc = sum(VOI_t2)/length(VOI_t2)*100
  VOI_t3_perc = sum(VOI_t3)/length(VOI_t3)*100
  VOI_t4_perc = sum(VOI_t4)/length(VOI_t4)*100
  VOI_t5_perc = sum(VOI_t5)/length(VOI_t5)*100
  return(rbind(intercept_perc,VOI_t0_perc,VOI_t1_perc,VOI_t2_perc,VOI_t3_perc,VOI_t4_perc,VOI_t5_perc))
}


basic_perc_1 = cbind(basic_perc(ni_out),basic_perc(ru_out),basic_perc(rb_out),basic_perc(cu_out),basic_perc(al_out))
basic_perc_1 = as.data.frame(basic_perc_1)
names(basic_perc_1) = c("NI", "RU", "RB", "CU", "AL")
write.csv(basic_perc_1, "aaaaaaa_pos_sig_perc.csv")




ni_out_1 = rbind(improved_output('ni1705','d20170301.csv',1),improved_output('ni1705','d20170302.csv',1),improved_output('ni1705','d20170303.csv',1),improved_output('ni1705','d20170306.csv',1),
                 improved_output('ni1705','d20170307.csv',1),improved_output('ni1705','d20170308.csv',1),improved_output('ni1705','d20170309.csv',1),improved_output('ni1705','d20170310.csv',1),
                 improved_output('ni1705','d20170313.csv',1),improved_output('ni1705','d20170314.csv',1),improved_output('ni1705','d20170315.csv',1),improved_output('ni1705','d20170316.csv',1),
                 improved_output('ni1705','d20170317.csv',1),improved_output('ni1705','d20170320.csv',1),improved_output('ni1705','d20170321.csv',1),improved_output('ni1705','d20170322.csv',1),
                 improved_output('ni1705','d20170323.csv',1),improved_output('ni1705','d20170324.csv',1),improved_output('ni1705','d20170327.csv',1),improved_output('ni1705','d20170328.csv',1),
                 improved_output('ni1705','d20170329.csv',1),improved_output('ni1705','d20170330.csv',1),improved_output('ni1705','d20170331.csv',1))
ni_out_1 = as.data.frame(ni_out_1,row.names = FALSE)
ni_out_1$pos_sig = ifelse(ni_out_1$coefficient>0 &ni_out_1$p_value<0.05,1,0)
ni_out_1$neg_sig = ifelse(ni_out_1$coefficient< 0 &ni_out_1$p_value<0.05,1,0)






al_out_1 = rbind(improved_output('al1705','d20170301.csv',5),improved_output('al1705','d20170302.csv',5),improved_output('al1705','d20170303.csv',5),improved_output('al1705','d20170306.csv',5),
                 improved_output('al1705','d20170307.csv',5),improved_output('al1705','d20170308.csv',5),improved_output('al1705','d20170309.csv',5),improved_output('al1705','d20170310.csv',5),
                 improved_output('al1705','d20170313.csv',5),improved_output('al1705','d20170314.csv',5),improved_output('al1705','d20170315.csv',5),improved_output('al1705','d20170316.csv',5),
                 improved_output('al1705','d20170317.csv',5),improved_output('al1705','d20170320.csv',5),improved_output('al1705','d20170321.csv',5),improved_output('al1705','d20170322.csv',5),
                 improved_output('al1705','d20170323.csv',5),improved_output('al1705','d20170324.csv',5),improved_output('al1705','d20170327.csv',5),improved_output('al1705','d20170328.csv',5),
                 improved_output('al1705','d20170329.csv',5),improved_output('al1705','d20170330.csv',5),improved_output('al1705','d20170331.csv',5))
al_out_1 = as.data.frame(al_out_1,row.names = FALSE)
al_out_1$pos_sig = ifelse(al_out_1$coefficient>0 & al_out_1$p_value<0.05,1,0)
al_out_1$neg_sig = ifelse(al_out_1$coefficient< 0 &al_out_1$p_value<0.05,1,0)

cu_out_1 = rbind(improved_output('cu1705','d20170301.csv',5),improved_output('cu1705','d20170302.csv',5),improved_output('cu1705','d20170303.csv',5),improved_output('cu1705','d20170306.csv',5),
                 improved_output('cu1705','d20170307.csv',5),improved_output('cu1705','d20170308.csv',5),improved_output('cu1705','d20170309.csv',5),improved_output('cu1705','d20170310.csv',5),
                 improved_output('cu1705','d20170313.csv',5),improved_output('cu1705','d20170314.csv',5),improved_output('cu1705','d20170315.csv',5),improved_output('cu1705','d20170316.csv',5),
                 improved_output('cu1705','d20170317.csv',5),improved_output('cu1705','d20170320.csv',5),improved_output('cu1705','d20170321.csv',5),improved_output('cu1705','d20170322.csv',5),
                 improved_output('cu1705','d20170323.csv',5),improved_output('cu1705','d20170324.csv',5),improved_output('cu1705','d20170327.csv',5),improved_output('cu1705','d20170328.csv',5),
                 improved_output('cu1705','d20170329.csv',5),improved_output('cu1705','d20170330.csv',5),improved_output('cu1705','d20170331.csv',5))
cu_out_1 = as.data.frame(cu_out_1,row.names = FALSE)
cu_out_1$pos_sig = ifelse(cu_out_1$coefficient>0 & cu_out_1$p_value<0.05,1,0)
cu_out_1$neg_sig = ifelse(cu_out_1$coefficient< 0 &cu_out_1$p_value<0.05,1,0)

ru_out_1 = rbind(improved_output('ru1705','d20170301.csv',10),improved_output('ru1705','d20170302.csv',10),improved_output('ru1705','d20170303.csv',10),improved_output('ru1705','d20170306.csv',10),
                 improved_output('ru1705','d20170307.csv',10),improved_output('ru1705','d20170308.csv',10),improved_output('ru1705','d20170309.csv',10),improved_output('ru1705','d20170310.csv',10),
                 improved_output('ru1705','d20170313.csv',10),improved_output('ru1705','d20170314.csv',10),improved_output('ru1705','d20170315.csv',10),improved_output('ru1705','d20170316.csv',10),
                 improved_output('ru1705','d20170317.csv',10),improved_output('ru1705','d20170320.csv',10),improved_output('ru1705','d20170321.csv',10),improved_output('ru1705','d20170322.csv',10),
                 improved_output('ru1705','d20170323.csv',10),improved_output('ru1705','d20170324.csv',10),improved_output('ru1705','d20170327.csv',10),improved_output('ru1705','d20170328.csv',10),
                 improved_output('ru1705','d20170329.csv',10),improved_output('ru1705','d20170330.csv',10),improved_output('ru1705','d20170331.csv',10))
ru_out_1 = as.data.frame(ru_out_1,row.names = FALSE)
ru_out_1$pos_sig = ifelse(ru_out_1$coefficient>0 & ru_out_1$p_value<0.05,1,0)
ru_out_1$neg_sig = ifelse(ru_out_1$coefficient< 0 & ru_out_1$p_value<0.05,1,0)

rb_out_1 = rbind(improved_output('rb1705','d20170301.csv',10),improved_output('rb1705','d20170302.csv',10),improved_output('rb1705','d20170303.csv',10),improved_output('rb1705','d20170306.csv',10),
                 improved_output('rb1705','d20170307.csv',10),improved_output('rb1705','d20170308.csv',10),improved_output('rb1705','d20170309.csv',10),improved_output('rb1705','d20170310.csv',10),
                 improved_output('rb1705','d20170313.csv',10),improved_output('rb1705','d20170314.csv',10),improved_output('rb1705','d20170315.csv',10),improved_output('rb1705','d20170316.csv',10),
                 improved_output('rb1705','d20170317.csv',10),improved_output('rb1705','d20170320.csv',10),improved_output('rb1705','d20170321.csv',10),improved_output('rb1705','d20170322.csv',10),
                 improved_output('rb1705','d20170323.csv',10),improved_output('rb1705','d20170324.csv',10),improved_output('rb1705','d20170327.csv',10),improved_output('rb1705','d20170328.csv',10),
                 improved_output('rb1705','d20170329.csv',10),improved_output('rb1705','d20170330.csv',10),improved_output('rb1705','d20170331.csv',10))
rb_out_1 = as.data.frame(rb_out_1,row.names = FALSE)
rb_out_1$pos_sig = ifelse(rb_out_1$coefficient>0 & rb_out_1$p_value<0.05,1,0)
rb_out_1$neg_sig = ifelse(rb_out_1$coefficient< 0 &rb_out_1$p_value<0.05,1,0)


#Calculate percentage of variables when their coefficient are positive and singificant
#Use improved model
improved_perc = function(dataset){
  intercept = dataset$pos_sig[seq(1,length(dataset$pos_sig),14)]
  VOI_t0 = dataset$pos_sig[seq(2,length(dataset$pos_sig),14)]
  VOI_t1 = dataset$pos_sig[seq(3,length(dataset$pos_sig),14)]
  VOI_t2 = dataset$pos_sig[seq(4,length(dataset$pos_sig),14)]
  VOI_t3 = dataset$pos_sig[seq(5,length(dataset$pos_sig),14)]
  VOI_t4 = dataset$pos_sig[seq(6,length(dataset$pos_sig),14)]
  VOI_t5 = dataset$pos_sig[seq(7,length(dataset$pos_sig),14)]
  OIR_t0 = dataset$pos_sig[seq(8,length(dataset$pos_sig),14)]
  OIR_t1 = dataset$pos_sig[seq(9,length(dataset$pos_sig),14)]
  OIR_t2 = dataset$pos_sig[seq(10,length(dataset$pos_sig),14)]
  OIR_t3 = dataset$pos_sig[seq(11,length(dataset$pos_sig),14)]
  OIR_t4 = dataset$pos_sig[seq(12,length(dataset$pos_sig),14)]
  OIR_t5 = dataset$pos_sig[seq(13,length(dataset$pos_sig),14)]
  MPB = dataset$pos_sig[seq(14,length(dataset$pos_sig),14)]
  
  intercept_perc = sum(intercept)/length(intercept)
  VOI_t0_perc = sum(VOI_t0)/length(VOI_t0)*100
  VOI_t1_perc = sum(VOI_t1)/length(VOI_t1)*100
  VOI_t2_perc = sum(VOI_t2)/length(VOI_t2)*100
  VOI_t3_perc = sum(VOI_t3)/length(VOI_t3)*100
  VOI_t4_perc = sum(VOI_t4)/length(VOI_t4)*100
  VOI_t5_perc = sum(VOI_t5)/length(VOI_t5)*100
  OIR_t0_perc = sum(OIR_t0)/length(OIR_t0)*100
  OIR_t1_perc = sum(OIR_t1)/length(OIR_t1)*100
  OIR_t2_perc = sum(OIR_t2)/length(OIR_t2)*100
  OIR_t3_perc = sum(OIR_t3)/length(OIR_t3)*100
  OIR_t4_perc = sum(OIR_t4)/length(OIR_t4)*100
  OIR_t5_perc = sum(OIR_t5)/length(OIR_t5)*100
  MPB_perc = sum(MPB)/length(MPB)*100
  return(rbind(intercept_perc,VOI_t0_perc,VOI_t1_perc,VOI_t2_perc,VOI_t3_perc,VOI_t4_perc,VOI_t5_perc,
               OIR_t0_perc,OIR_t1_perc,OIR_t2_perc,OIR_t3_perc,OIR_t4_perc,OIR_t5_perc,MPB_perc))
}

improved_OIR_perc = function(dataset){
  OIR_t0 = dataset$neg_sig[seq(8,length(dataset$neg_sig),14)]
  OIR_t1 = dataset$neg_sig[seq(9,length(dataset$neg_sig),14)]
  OIR_t2 = dataset$neg_sig[seq(10,length(dataset$neg_sig),14)]
  OIR_t3 = dataset$neg_sig[seq(11,length(dataset$neg_sig),14)]
  OIR_t4 = dataset$neg_sig[seq(12,length(dataset$neg_sig),14)]
  OIR_t5 = dataset$neg_sig[seq(13,length(dataset$neg_sig),14)]
  

  OIR_t1_perc = sum(OIR_t1)/length(OIR_t1)*100
  OIR_t2_perc = sum(OIR_t2)/length(OIR_t2)*100
  OIR_t3_perc = sum(OIR_t3)/length(OIR_t3)*100
  OIR_t4_perc = sum(OIR_t4)/length(OIR_t4)*100
  OIR_t5_perc = sum(OIR_t5)/length(OIR_t5)*100
  return(rbind(OIR_t1_perc,OIR_t2_perc,OIR_t3_perc,OIR_t4_perc,OIR_t5_perc))
}

improved_perc_1 = cbind(improved_perc(ni_out_1),improved_perc(ru_out_1),improved_perc(rb_out_1),improved_perc(cu_out_1),improved_perc(al_out_1))
improved_perc_1 = as.data.frame(improved_perc_1,row.names = FALSE)
names(improved_perc_1) = c("NI", "RU", "RB", "CU", "AL")
write.csv(improved_perc_1, "improved_pos_sig_perc.csv")

#Calculate percentage for OIR when its coefficient is negative but singificant
#Use improved model

#OIR negative & significant 
improved_OIR_perc = cbind(improved_OIR_perc(ni_out_1),improved_OIR_perc(ru_out_1),improved_OIR_perc(rb_out_1),improved_OIR_perc(cu_out_1),improved_OIR_perc(al_out_1))
improved_OIR_perc = as.data.frame(improved_OIR_perc,row.names = FALSE)
names(improved_OIR_perc) = c("NI", "RU", "RB", "CU", "AL")
write.csv(improved_perc, "improved_OIR_perc.csv")




