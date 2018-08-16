##### Random forest #################################################
rf.fit <- randomForest(RENEWAL_STATUS__c ~ ., data = train, ntree = 300, mtry = 10, importance = TRUE)
print(rf.fit)
print(importance(rf.fit,type = 2))
plot(rf.fit) # Error rate is decreasing or inreasing
predTrain <- predict(rf.fit, train[,-32], type = "class")
predTrain = as.data.frame(predTrain)
predTrain$predTrain <- round(predTrain$predTrain)


# Handling varible importance
indices <- which(varImp(rf.fit)$Overall >= 20)
new_train <- train[,c(indices, ncol(train))]
new_test <- test[,c(indices, ncol(test))]
rf.fit <- randomForest(RENEWAL_STATUS__c ~ ., data = new_train, ntree = 300, mtry = 10, importance = TRUE)

predTest <- predict(rf.fit, new_test[,-(ncol(new_test))], type = "class")
predTest = as.data.frame(predTest)
predTest$predTest <- round(predTest$predTest)

table(new_test$RENEWAL_STATUS__c, predTest$predTest)
