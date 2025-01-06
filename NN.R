rm(list = ls())
data ="Eco"
pred_type = "spatIntpl"

# Load necessary libraries
library(reticulate)
use_virtualenv("~/r-keras-env", required = TRUE)

library(keras)
#install_keras(method = "virtualenv", envname = "~/r-keras-env")

### data of interests
library(sparseDGLM)
if(data =="Eco"){
  data("Montreal_EcoCounter_bike_volume")
  Y<- EcoCounter_bike_counts_data_2022$Y
  X<- EcoCounter_bike_counts_data_2022$X.array
  loc<- EcoCounter_bike_counts_data_2022$loc
  
  spatInt.ind = c(2, 11, 20, 39, 44)
  forcast.ind = 1:7
}else{
  data("Portland_traffic_volume")
  Y<- Portland_traffic_volume_data_2024$Y
  X<- Portland_traffic_volume_data_2024$X.array
  loc<- Portland_traffic_volume_data_2024$loc
  
  set.seed(1)
  spatInt.ind = sample(1:ncol(Y), size =10, replace =FALSE)
  forcast.ind = 1:7
}

nt<- nrow(Y)
ns<- ncol(Y)
nt.train<- nt - length(forcast.ind)


### defining predictor variables to catch the seasonality
x.sin<- sin(2*pi * (1:nt) / 7)
x.cos<- cos(2 * pi * (1:nt) /7)
p<- dim(X)[3]
X1<- abind::abind(X, matrix(rep(x.sin, ns), nrow = nt, ncol = ns), along = 3)
X2<- abind::abind(X1, matrix(rep(x.cos, ns), nrow = nt, ncol = ns), along = 3)

X<- X2
dim(X)

if(pred_type == "spatIntpl"){
Y.train<- Y[ ,-spatInt.ind]
Y.test<- Y[ ,spatInt.ind]

X.train<- X[, -spatInt.ind, ]
X.test<-  X[, spatInt.ind, ]

} else if(pred_type == "forecast"){
  Y.train<- Y[1:nt.train, ]
  Y.test<- Y[(nt.train + 1):nt, ]
  
  X.train<- X[1:nt.train, , ]
  X.test<-  X[(nt.train + 1):nt, , ] 
  
} else if(pred_type == "spaceTime"){
  Y.train<- Y[1:nt.train, -spatInt.ind]
  Y.test<- Y[(nt.train + 1):nt, spatInt.ind ]
  
  X.train<- X[1:nt.train, -spatInt.ind, ]
  X.test<-  X[(nt.train + 1):nt, spatInt.ind, ] 
}

X_train<- apply(array(X.train, dim = c(dim(X.train)[1] * dim(X.train)[2],  dim(X.train)[3])),
                MARGIN = 2, FUN = function(x) x/max(x, na.rm=TRUE)) ## normalizing each predictor variables 
Y_train <- c(Y.train)
X_test<- apply(array(X.test, dim = c(dim(X.test)[1] * dim(X.test)[2],  dim(X.test)[3])),
               MARGIN = 2, FUN = function(x) x/max(x, na.rm = TRUE)) ## normalizing each predictor variables 
Y_test<- c(Y.test)

### remove any NAs in response variables 
na.ind<- is.na(Y_train)
Y_train<- Y_train[!na.ind]
X_train<- X_train[!na.ind, ]


model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train)) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = "adam",
  loss = "mean_squared_error"
)

history <- model %>% fit(
  X_train, Y_train,
  epochs = 100,
  batch_size = 32,
  validation_split = 0.2
)

plot(history)

test_loss <- model %>% evaluate(X_test, Y_test)
cat("Test loss:", test_loss, "\n")

predictions <- model %>% predict(X_test)
head(predictions)
head(Y_test)
