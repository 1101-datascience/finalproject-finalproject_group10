##modeling.R

args = commandArgs(trailingOnly=TRUE)
# parse parameters
i<-1
while(i < length(args))
{
  if(args[i] == "--train"){
    train_data<-args[i+1]
    i<-i+1
  }else if(args[i] == "--test"){
    test_data<-args[i+1]
    i<-i+1
  }else if(args[i] == "--report"){
    report_dist<-args[i+1]
    i<-i+1
  }else if(args[i] == "--predict"){
    predict_dist<-args[i+1]
    i<-i+1
  }
  i<-i+1
}

### input data
d <- read.csv(train_data, header = T,stringsAsFactors = FALSE)

##random forest
require(randomForest)
library(randomForest)
set.seed(100)
select.index <- sample(x=1:nrow(d), size=50000)

train.index <- sample(x=select.index, size=ceiling(0.8*50000 ))
train <- d[train.index, ]
test <- d[-train.index, ]
mse_test<-c()
r2_test<-c()

## model1
rf_model1 = randomForest(target~.,
                        data=train,
                        ntree=500,
                        mtry=54
)
rf_y <- predict(rf_model1, test)
mse_test <- c(mse_test,mean((rf_y - test$target)^2)) # MSE
r2_test<-c(r2_test,1 - (sum((test$target - rf_y)^2) / sum((test$target-mean(test$target))^2)) )
saveRDS(rf_model1, "rf_model1.rds")

## model2
set.seed(222)
select.index <- sample(x=1:nrow(d), size=50000)
train.index <- sample(x=select.index, size=ceiling(0.8*50000 ))
train <- d[train.index, ]
test <- d[-train.index, ]

rf_model2 = randomForest(target~.,
                         data=train,
                         ntree=500,
                         mtry=54
)
rf_y <- predict(rf_model2, test)
mse_test <- c(mse_test,mean((rf_y - test$target)^2)) # MSE
r2_test<-c(r2_test,1 - (sum((test$target - rf_y)^2) / sum((test$target-mean(test$target))^2)) )
saveRDS(rf_model2, "rf_model2.rds")

## model3
set.seed(33)
select.index <- sample(x=1:nrow(d), size=50000)
train.index <- sample(x=select.index, size=ceiling(0.8*50000 ))
train <- d[train.index, ]
test <- d[-train.index, ]

rf_model3 = randomForest(target~.,
                         data=train,
                         ntree=500,
                         mtry=54
)
rf_y <- predict(rf_model3, test)
mse_test <- c(mse_test,mean((rf_y - test$target)^2)) # MSE
r2_test<-c(r2_test,1 - (sum((test$target - rf_y)^2) / sum((test$target-mean(test$target))^2)) )
saveRDS(rf_model3, "rf_model3.rds")

##model 4
set.seed(44)
select.index <- sample(x=1:nrow(d), size=50000)
train.index <- sample(x=select.index, size=ceiling(0.8*50000 ))
train <- d[train.index, ]
test <- d[-train.index, ]

rf_model4 = randomForest(target~.,
                         data=train,
                         ntree=500,
                         mtry=54
)
rf_y <- predict(rf_model4, test)
mse_test <- c(mse_test,mean((rf_y - test$target)^2)) # MSE
r2_test<-c(r2_test,1 - (sum((test$target - rf_y)^2) / sum((test$target-mean(test$target))^2)) )
saveRDS(rf_model4, "rf_model4.rds")

##model 5
set.seed(77)
select.index <- sample(x=1:nrow(d), size=50000)
train.index <- sample(x=select.index, size=ceiling(0.8*50000 ))
train <- d[train.index, ]
test <- d[-train.index, ]

rf_model5 = randomForest(target~.,
                         data=train,
                         ntree=500,
                         mtry=54
)
rf_y <- predict(rf_model5, test)
mse_test <- c(mse_test,mean((rf_y - test$target)^2)) # MSE
r2_test<-c(r2_test,1 - (sum((test$target - rf_y)^2) / sum((test$target-mean(test$target))^2)) )
saveRDS(rf_model5, "rf_model5.rds")

## RMSE
rmse_test <-c()
for(i in 1:5){
  rmse_test<- c(rmse_test,sqrt(mse_test[i])) #RMSE
}
## r-squared      R_squared = 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)


## adjust model
print(tuneRF(train[,-9], train[,9]))
plot(rf_model5)
dev.copy(jpeg,filename="plot.jpg");
dev.off ();

## performance
out_performance<-data.frame(
  model=paste("model",1:5,  sep = ""), 
  MSE=mse_test,
  RMSE=rmse_test,
  R_squared=r2_test,
  stringsAsFactors = F
)
write.csv(out_performance ,report_dist, row.names = F, quote = F)

## input + predict test data
d_test <- read.csv(test_data, header = T,stringsAsFactors = FALSE)
rf_test1 = predict(rf_model1, d_test)
rf_test2 = predict(rf_model2, d_test)
rf_test3 = predict(rf_model3, d_test)
rf_test4 = predict(rf_model4, d_test)
rf_test5 = predict(rf_model5, d_test)

mean_target<-c()
for(i in 1:nrow(d_test)){
  m<-c(rf_test1[i],rf_test2[i],rf_test3[i],rf_test4[i],rf_test5[i])
  m1<-m[-which.max(m)]
  m2<-m1[-which.min(m1)]
  mean_target<-c(mean_target,mean(m2))
}

## output result
out_predict<-data.frame(
  card_id=d_test$card_id,
  target=mean_target,
  stringsAsFactors = F
)
write.csv(out_predict ,predict_dist, row.names = F, quote = F)

