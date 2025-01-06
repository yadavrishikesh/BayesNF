rm(list = ls())
setwd(this.path::here())
library(sparseDGLM)
library(lubridate)

data_for_bayesnf_new<- function(data, pred_type, missing_props){

  data("EcoCounter_bike_data_2021_2022")
  Y.all<- daily_bike_data_2021_2022$bikecounts #log(1+daily_bike_data$bikecounts)
  Y.all[Y.all <10]<- NA
  Y<- Y.all
  ns<- ncol(Y)
  nt<- nrow(Y)

  set.seed(1)
  spatInt.ind = c(11, 34, 41, 44)
  miss.artfical.na<- c(sort(sample(setdiff(1:(ncol(daily_bike_data_2021_2022$bikecounts)-4), spatInt.ind),
                                   size = 4, replace = TRUE), decreasing = FALSE))
  num_sites <- length(miss.artfical.na)
  missing_indices <- list()
  set.seed(123)
  generate_consecutive_indices <- function(total_days, num_missing) {
    missing_indices <- numeric(0)
    remaining_days <- total_days
    while (length(missing_indices) < num_missing) {
      block_size <- sample(1:min(30, num_missing - length(missing_indices)), 1)
      start <- sample(setdiff(1:remaining_days, missing_indices), 1)
      block <- start:(start + block_size - 1)
      block <- block[block <= total_days]
      missing_indices <- unique(c(missing_indices, block))
    }
    return(sort(missing_indices[1:num_missing]))
  }

  for (site in 1:num_sites) {
    missing_percentage <- missing_props
    num_missing <- round((missing_percentage / 100) * nt)
    missing_indices[[site]] <- generate_consecutive_indices(nt, num_missing)
  }

  for (i in 1:length(miss.artfical.na)) {
    Y[missing_indices[[i]], miss.artfical.na[i]]<- NA  ## creating artificial NA's
  }

  X<- array(dim = c(nrow(Y), ncol(Y), 9))
  X[,,1]<- matrix((daily_bike_data_2021_2022$purely_temp_cov$temp), nrow = nrow(Y), ncol=ncol(Y))
  X[,,2]<- matrix((daily_bike_data_2021_2022$purely_temp_cov$visbl), nrow = nrow(Y), ncol=ncol(Y))
  X[,,3]<- matrix((daily_bike_data_2021_2022$purely_temp_cov$wsp), nrow = nrow(Y), ncol=ncol(Y))
  X[,,4]<- matrix(daily_bike_data_2021_2022$purely_temp_cov$precp.dummy, nrow = nrow(Y), ncol=ncol(Y))
  X[,,5]<- matrix(daily_bike_data_2021_2022$purely_temp_cov$weeknd.dummy, nrow = nrow(Y), ncol=ncol(Y))
  X[,,6]<- matrix(daily_bike_data_2021_2022$purely_temp_cov$year.dummy, nrow = nrow(Y), ncol=ncol(Y))
  X[,,7]<- matrix(rep(daily_bike_data_2021_2022$purely_spatial_cov$elev,
                       each = nrow(Y)), nrow = nrow(Y), ncol=ncol(Y))
  X[,,8]<- matrix(rep(daily_bike_data_2021_2022$purely_spatial_cov$walkscore,
                       each = nrow(Y)), nrow = nrow(Y), ncol=ncol(Y))
  X[,,9]<- matrix(rep(daily_bike_data_2021_2022$purely_spatial_cov$num_ppo,
                       each = nrow(Y)), nrow = nrow(Y), ncol=ncol(Y))
  dimnames(X)<- list(NULL, NULL, c("temp", "visbl", "wsp", "precp.dummy", "weeknd.dummy",
                                   paste0("year.dummy-",2022), "elev", "walkscore", "num_ppo"))

  X<- abind::abind("datetime" = matrix(rep(c(seq.Date(from = as.Date("2021-04-15"), to = as.Date("2021-11-15"), by = "day"),
                                             seq.Date(from = as.Date("2022-04-15"), to = as.Date("2022-11-15"), by = "day")),
                                           ns), ncol = ns, nrow = nt), X , along = 3)

  loc<- daily_bike_data_2021_2022$loc
  forcast.ind = 1:14

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
  train_data$datetime<- as.Date(train_data$datetime, origin = "1970-01-01")

  write.csv(train_data,
            file = paste0("data/train_data_", data, "_pred-type_", pred_type, "_miss_prop_", missing_props, ".csv"),  row.names = TRUE)

  X_test<- cbind(c(X.test[,,1]), apply(array(X.test[,,-1], dim = c(dim(X.test)[1] * dim(X.test)[2],  dim(X.test)[3]-1)),
                                       MARGIN = 2, FUN = function(x) x/max(x, na.rm = TRUE))) ## normalizing each predictor variables
  Y_test<- c(Y.test)

  test_data<- cbind.data.frame(X_test,
                               rep(loc.test[,1], each = nrow(Y.test) ), rep(loc.test[,2], each =nrow(Y.test)),
                               Y_test)
  colnames(test_data)<- c(dimnames(X)[[3]], "lon", "lat", "response")
  test_data$datetime<- as.Date(test_data$datetime, origin = "1970-01-01")
  write.csv(test_data, file = paste0("data/test_data_",
                                     data, "_pred-type_", pred_type, "_miss_prop_", missing_props, ".csv"),  row.names = TRUE)
}

data_for_bayesnf_new(data ="Eco_2017-2022", pred_type = "spatIntpl", missing_props  = 50)
data_for_bayesnf_new(data ="Eco_2017-2022", pred_type = "spatIntpl", missing_props = 60)
data_for_bayesnf_new(data ="Eco_2017-2022", pred_type = "spatIntpl", missing_props = 80)



