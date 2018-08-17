
library("randomForest")
library("xgboost")
library(Matrix)
library(data.table)
library("caret")
library(e1071)
library(HH)


################ Logistic regression

logistic_regression <- function(train, test){
model <- glm (RENEWAL_STATUS__c ~ ., data = train, family = binomial(link="logit"))

predicted <- predict(model, test[,-32], type="response")
glm_predicted = as.data.frame(predicted)
glm_predicted$predicted <- ifelse(glm_predicted$predicted >= 0.5, 1,0)

table(test$RENEWAL_STATUS__c, glm_predicted$predicted)

# Finding Multi-collinearity using Variance Inflation Factor
vif_score <- vif(model1)
removed_vif <- as.data.frame(vif_score)
for (i in 1:nrow(removed_vif)) {
  if(removed_vif$vif_score[i] > 5){
     removed_vif$vif[i] <- "TRUE"
    } else{
      removed_vif$vif[i] <- "FALSE"
    }
}

indices <- which(removed_vif$vif == FALSE)
glm_train <- train[,c(indices, ncol(train))]
glm_test <- test[,c(indices, ncol(test))]


model1 <- glm (RENEWAL_STATUS__c ~ ., data = glm_train, family = binomial(link="logit"))

predicted <- predict(model1, glm_test[,-c(ncol(glm_test))], type="response")
glm_predicted = as.data.frame(predicted)
glm_predicted$predicted <- ifelse(glm_predicted$predicted >= 0.5, 1,0)

table(glm_test$RENEWAL_STATUS__c, glm_predicted$predicted)

}




