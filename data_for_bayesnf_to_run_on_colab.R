rm(list = ls())
setwd(this.path::here())
library(sparseDGLM)
library(lubridate)

data_for_bayesnf<- function(data, pred_type){
 #browser()
  if(data =="Eco"){
    data("EcoCounter_bike_data")
    Y.all<- daily_bike_data$bikecounts #log(1+daily_bike_data$bikecounts)
    miss.artfical.na<-  list(site1 = 1:20, site15 = 200:230,
                             site25 = 350:400, site40 = 100:150)
    Y<- Y.all
    Y[miss.artfical.na$site1, 1]<- NA
    Y[miss.artfical.na$site15, 15]<- NA
    Y[miss.artfical.na$site25, 25]<- NA
    Y[miss.artfical.na$site40, 40]<- NA
    
    ns.all<- ncol(Y)
    nt.all<- nrow(Y)
    
    X<- array(dim = c(nrow(Y), ncol(Y), 9))
    X[,,1]<- matrix((daily_bike_data$purely_temp_cov$temp), nrow = nrow(Y), ncol=ncol(Y))
    X[,,2]<- matrix((daily_bike_data$purely_temp_cov$visbl), nrow = nrow(Y), ncol=ncol(Y))
    X[,,3]<- matrix((daily_bike_data$purely_temp_cov$wsp), nrow = nrow(Y), ncol=ncol(Y))
    X[,,4]<- matrix(daily_bike_data$purely_temp_cov$precp.dummy, nrow = nrow(Y), ncol=ncol(Y))
    X[,,5]<- matrix(daily_bike_data$purely_temp_cov$weeknd.dummy, nrow = nrow(Y), ncol=ncol(Y))
    X[,,6]<- matrix(daily_bike_data$purely_temp_cov$year.dummy, nrow = nrow(Y), ncol=ncol(Y))
    X[,,7]<- matrix(rep((daily_bike_data$purely_spatial_cov$elev), each = nrow(Y)), nrow = nrow(Y), ncol=ncol(Y))
    X[,,8]<- matrix(rep((daily_bike_data$purely_spatial_cov$walkscore), each = nrow(Y)), nrow = nrow(Y), ncol=ncol(Y))
    X[,,9]<- matrix(rep((daily_bike_data$purely_spatial_cov$num_ppo), each = nrow(Y)), nrow = nrow(Y), ncol=ncol(Y))
    dimnames(X)<- list(NULL, NULL, c("temp", "visbl", "wsp", "precp.dummy", "weeknd.dummy",
                                     "year.dummy", "elev", "walkscore", "num_ppo"))
    X<- abind::abind("datetime" = matrix(rep(c(seq.Date(from = as.Date("2021-04-15"), to = as.Date("2021-11-15"), by = "day"),
                                               seq.Date(from = as.Date("2022-04-15"), to = as.Date("2022-11-15"), by = "day")),
                                             ns.all), ncol = ns.all, nrow = nt.all),
                     X , along = 3)
    
    loc<- daily_bike_data$loc
    
    spatInt.ind = c(11, 34, 41, 44)
    forcast.ind = 1:7 
    
  }else if(data=="Portland_hourly"){
    data("Portland_traffic_data")
    
    Y.all<- hourly_traffic_data$volume # log(1+hourly_traffic_data$volume)
    miss.artfical.na<-  list(site1 = 1:20, site15 = 200:216,
                             site25 = 20:50, site40 = 100:150)
    Y<- Y.all
    Y[miss.artfical.na$site1, 1]<- NA
    Y[miss.artfical.na$site15, 15]<- NA
    Y[miss.artfical.na$site25, 25]<- NA
    Y[miss.artfical.na$site40, 40]<- NA
    
    nt.all<- nrow(Y)
    ns.all<- ncol(Y)
    
    X<- array(dim = c(nrow(Y), ncol(Y), 6))
    X[,,1]<- hourly_traffic_data$speed
    X[,,2]<- hourly_traffic_data$occupancy
    X[,,3]<- hourly_traffic_data$vht
    X[,,4]<- hourly_traffic_data$vmt
    X[,,5]<- hourly_traffic_data$traveltime
    X[,,6]<- hourly_traffic_data$delay
    dimnames(X)<- list(NULL, NULL, 
                       c("speed", "occupancy", "vht", "vmt", "traveltime", "delay"))
    X<- abind::abind("datetime" = matrix(rep(seq.POSIXt(from = as.POSIXct("2024-01-01 00:00:00", tz = "EST"), 
                                                        to = as.POSIXct("2024-01-10 23:00:00", tz = "EST"), 
                                                        by = "hour"), ns.all), ncol = ns.all, nrow = nt.all),
                     X , along = 3)
    loc<- hourly_traffic_data$loc
    set.seed(111)
    spatInt.ind = sample(1:ncol(Y), size =10, replace =FALSE)
    forcast.ind = 1:24
  } else if(data=="Portland_daily"){
    
    data("Portland_traffic_data")
    
    Y.all<- daily_traffic_data$volume # log(1+daily_traffic_data$volume)
    miss.artfical.na<-  list(site1 = 1:20, site15 = 100:116,
                             site25 = 20:50, site40 = 100:130)
    Y<- Y.all
    Y[miss.artfical.na$site1, 1]<- NA
    Y[miss.artfical.na$site15, 15]<- NA
    Y[miss.artfical.na$site25, 25]<- NA
    Y[miss.artfical.na$site40, 40]<- NA
    
    nt.all<- nrow(Y)
    ns.all<- ncol(Y)
    
    X<- array(dim = c(nrow(Y), ncol(Y), 6))
    X[,,1]<- daily_traffic_data$speed
    X[,,2]<- daily_traffic_data$occupancy
    X[,,3]<- daily_traffic_data$vht
    X[,,4]<- daily_traffic_data$vmt
    X[,,5]<- daily_traffic_data$traveltime
    X[,,6]<- daily_traffic_data$delay
    dimnames(X)<- list(NULL, NULL, c("speed", "occupancy", "vht", "vmt", "traveltime",
                                     "delay"))
    X<- abind::abind("datetime" = matrix(rep(seq.Date(from = as.Date("2024-01-01"), to = as.Date("2024-05-29"), by = "day"),
                                             ns.all), ncol = ns.all, nrow = nt.all),
                     X , along = 3)
    
    loc<- daily_traffic_data$loc
    set.seed(111)
    spatInt.ind = sample(1:ncol(Y), size =10, replace =FALSE)
    forcast.ind = 1:7
  }
  
  nt<- nrow(Y)
  ns<- ncol(Y)
  nt.train<- nt - length(forcast.ind)
  
  if(pred_type == "spatIntpl"){
    Y.train<- Y[ ,-spatInt.ind]
    Y.test<- Y[ ,spatInt.ind]
    
    loc.train<- loc[-spatInt.ind,]
    
    X.train<- X[, -spatInt.ind, ]
    X.test<-  X[, spatInt.ind, ]
    loc.test<- loc[spatInt.ind,]
  } else if(pred_type == "forecast"){
    Y.train<- Y[1:nt.train, ]
    Y.test<- Y[(nt.train + 1):nt, ]
    loc.train<- loc
    
    X.train<- X[1:nt.train, , ]
    X.test<-  X[(nt.train + 1):nt, , ] 
    loc.test<- loc
    
  } else if(pred_type == "spaceTime"){
    Y.train<- Y[1:nt.train, -spatInt.ind]
    Y.test<- Y[(nt.train + 1):nt, spatInt.ind ]
    loc.train<- loc[-spatInt.ind,]
    
    X.train<- X[1:nt.train, -spatInt.ind, ]
    X.test<-  X[(nt.train + 1):nt, spatInt.ind, ] 
    loc.test<- loc[spatInt.ind,]
  }
  
  X_train<- cbind(c(X.train[,,1]),  apply(array(X.train[,,-1], dim = c(dim(X.train)[1] * dim(X.train)[2],  dim(X.train)[3] -1)),
                                          MARGIN = 2, FUN = function(x) x/max(x, na.rm=TRUE))) ## normalizing each predictor variables 
  Y_train <- c(Y.train)
  
  train_data<- cbind.data.frame(X_train,
                                rep(loc.train[,1], each =nrow(Y.train)), rep(loc.train[,2], each =nrow(Y.train)),
                                Y_train)
  colnames(train_data)<- c(dimnames(X)[[3]], "lon", "lat",  "response")
  na.ind<- is.na(train_data$response)
  train_data<- train_data[!na.ind,]
  if(data =="Portland_hourly"){
  train_data$datetime<- as.POSIXct(train_data$datetime, origin = "1970-01-01", tz="EST")
  train_data$datetime <- strftime(train_data$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "EST")
  } else{
  train_data$datetime<- as.Date(train_data$datetime, origin = "1970-01-01")
  }
  
  write.csv(train_data, file = paste0("data/train_data_", data, "_pred-type_", pred_type, ".csv"),  row.names = TRUE)
  
  X_test<- cbind(c(X.test[,,1]), apply(array(X.test[,,-1], dim = c(dim(X.test)[1] * dim(X.test)[2],  dim(X.test)[3]-1)),
                                       MARGIN = 2, FUN = function(x) x/max(x, na.rm = TRUE))) ## normalizing each predictor variables 
  Y_test<- c(Y.test)
  
  test_data<- cbind.data.frame(X_test,
                               rep(loc.test[,1], each = nrow(Y.test) ), rep(loc.test[,2], each =nrow(Y.test)),
                               Y_test)
  colnames(test_data)<- c(dimnames(X)[[3]], "lon", "lat", "response")
  if(data =="Portland_hourly"){
    test_data$datetime<- as.POSIXct(test_data$datetime, origin = "1970-01-01", tz = "EST")
    test_data$datetime <- strftime(test_data$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "EST")
  } else{
    test_data$datetime<- as.Date(test_data$datetime, origin = "1970-01-01")
    
  }
  write.csv(test_data, file = paste0("data/test_data_", data, "_pred-type_", pred_type, ".csv"),  row.names = TRUE)
}

### extarcting these datasets 
data_for_bayesnf(data ="Eco", pred_type = "spatIntpl")
data_for_bayesnf(data ="Eco", pred_type = "forecast")
data_for_bayesnf(data ="Eco", pred_type = "spaceTime")
data_for_bayesnf(data ="Portland_daily", pred_type = "spatIntpl")
data_for_bayesnf(data ="Portland_daily", pred_type = "forecast")
data_for_bayesnf(data ="Portland_daily", pred_type = "spaceTime")
data_for_bayesnf(data ="Portland_hourly", pred_type = "spatIntpl")
data_for_bayesnf(data ="Portland_hourly", pred_type = "forecast")
data_for_bayesnf(data ="Portland_hourly", pred_type = "spaceTime")



