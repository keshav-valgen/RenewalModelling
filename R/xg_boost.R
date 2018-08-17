library(xgboost)
library(Matrix)
# split train data and make xgb.DMatrix


train_data   <- train[,-32]
test_data <- test[,-32]
train_label  <- train$RENEWAL_STATUS__c


labels = as.numeric(train_label)
setDT(train_data)
setDT(test_data)

new_tr <- model.matrix(~.+0,data = train_data)
new_ts <- model.matrix(~.+0,data = test_data)

dtrain <- xgb.DMatrix(data = new_tr,label = labels)
dtest <- xgb.DMatrix(data = new_ts)


params <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta=0.2,
  gamma=0,
  max_depth=50,
  min_child_weight=10,
  subsample=0.5,
  max_delta_step = 5,
  colsample_bytree=1
)

#first default - model training
xgb1 <- xgb.train(
  params = params
  ,data = dtrain
  ,nrounds = 100
  ,eval_metric = "error"
)

data_predict = predict(xgb1 , dtest)
data_predict = as.data.frame(round(data_predict))
names(data_predict)[1] = "Predicted_data"
table(test$RENEWAL_STATUS__c, data_predict$Predicted_data)
