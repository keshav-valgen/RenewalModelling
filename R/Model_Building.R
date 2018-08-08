
library("randomForest")
library("xgboost")
######### Random Forest Model

rm.fit <- randomForest()
xg.fit <- xgboost()
################### Building the Multi Classification Model using xgboost ##########

# split train data and make xgb.DMatrix
train_data   <- train[,-32]
train_label  <- train$RENEWAL_STATUS__c
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)

# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)


# Creating parameters
numberOfClasses <- length(unique(new_data$RENEWAL_STATUS__c))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)

nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix,
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)
