
library("randomForest")
library("xgboost")
library(Matrix)
library(data.table)
library("caret")
library(e1071)
library(HH)


################ Logistic regression

model <- glm (RENEWAL_STATUS__c ~ ., data = train, family = binomial(link="logit"))
summary(model)

predicted <- predict(model, test[,-32], type="response")
glm_predicted = as.data.frame(predicted)
glm_predicted$predicted <- ifelse(glm_predicted$predicted >= 0.5, 1,0)

table(test$RENEWAL_STATUS__c, glm_predicted$predicted)

# Finding Multi-collinearity using Variance Inflation Factor
vif_score <- vif(model)
removed_vif <- as.data.frame(vif_score)


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




# Creating parameters
numberOfClasses <- 5
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)

nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = dtrain,
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)

# Train the model
bst_model <- xgb.train(params = xgb_params,
                       data = dtrain,
                       nrounds = nround)

# Predict hold-out test set
test_pred <- predict(bst_model, dtest)

data_predict = data.frame(round(test_pred))

