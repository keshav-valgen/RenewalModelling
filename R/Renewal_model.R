library(RForcecom)
library(dplyr)
library(sqldf)
library(Metrics)


  instance_u <- paste0(instance_url,'/')
  api <- '36.0'

  object <- "data"
  Customer_id <- "ACCT_ID__c"
  Order_id <- "ORDER_TYPE__c"
  product_name <- "Name"
  Date_purchase <- "ORDER_RENEWAL_DATE__c"
  Quantity <- "NET_UNITS__c"
  Unit_Price <- "PRICE_PER_UNIT__c"
  Amount <- "REVENUE__c"
  Others <- "a,b,c"


  myquery <- paste0('Select Id,',
                    Customer_id,',',
                    Order_id,', ',
                    product_name,', ',
                    Date_purchase,', ',
                    Quantity,', ',
                    Unit_Price,', ',
                    Amount,', ',Others,' FROM ', object)


  session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)
  data1 <- rforcecom.bulkQuery(session, myquery, object)

  # Separate all numeric & Categorical columns
  data_numeric = select_if(data2, is.numeric)
  data_categorical <- select_if(data2, is.factor)

  # Handling Missing values with Mean, Median, Mode

  misssing_mean <- function(data){
    data <- lapply(data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
    data <- data.frame(data)
    return(data)
  }


  misssing_median <- function(data){
    data <- lapply(data, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
    data <- data.frame(data)
    return(data)
  }

  misssing_mode <- function(data){
    for(i in 1:ncol(data)){
      if(is.factor(data[,i])){
        a = data[,i][which.max(data[,i])]
        a = data.frame(a)
        data[,i] <- ifelse(is.na(data[,i]),a$a,data[,i])
      }
    }
    return(data)
  }

  separate_missing_data <- function(data){
    new_DF <- data[rowSums(is.na(data)) > 0,]
    return(new_DF)
  }

  missing_mean <-  misssing_mean(data_numeric)
  missing_median <- misssing_median(data_numeric)
  missing_mode <- misssing_mode(data_numeric)
  missing_separate <- separate_missing_data(data2)
  missing_categorical <- misssing_mode(data_categorical)

  ####### Modification Required if data changes
  data = cbind(missing_mean,missing_categorical[,-3], data2$ORDER_RENEWAL_DATE__c)
  for (i in ncol(data)) {
    names(data)[i] <- "ORDER_RENEWAL_DATE__c"
  }

  ############################ Handling Outliers with automation #########
  outliers1 = function(data){
    for (i in 1:ncol(data)) {
      data[,i][data[,i] %in% boxplot.stats(data[,i])$out] <- median(data[,i])
    }
    return(data)
  }

  outlier_data = outliers1(data[,-22])

  data1 <- cbind(outlier_data, data2$ORDER_RENEWAL_DATE__c)

  for (i in ncol(data1)) {
    names(data1)[i] <- "ORDER_RENEWAL_DATE__c"
  }

source("R/rfmp_derived_variables.R")
     output = prediction(data1$ACCT_ID__c,
                       data1$ORDER_TYPE__c,
                       data1$Name,
                       data1$ORDER_RENEWAL_DATE__c,
                       data1$NET_UNITS__c,
                       data1$PRICE_PER_UNIT__c,
                       data1$REVENUE__c)


############ Finding sparse value variables
  sparse <- function(output){
    a = c()
    for (i in 1:ncol(output)) {
      count3 <- length(which(output[,i] == 0))
      if(count3 > 0){
        per <- (count3/nrow(output))*100
      } else {
        per = 0
      }
      if(per > 95){
        a[[i]] <- "TRUE"
      } else {
        a[[i]] <- "FALSE"
      }
    }
    return(a)
  }

  processed_data <- sparse(output)

  ########## Remove Sparse Variables
  for (i in 1:ncol(output)) {
    if (processed_data[i] == "TRUE") {
      output <- output[,-i]
    } else {
      next
    }
  }


  new_data <- cbind(output, data2$RENEWAL_STATUS__c)

  for (i in ncol(new_data)) {
    names(new_data)[i] <- "RENEWAL_STATUS__c"
  }


  #### Handling dependent variables
  new_data$RENEWAL_STATUS__c <- ifelse(new_data$RENEWAL_STATUS__c == "AUTO" | new_data$RENEWAL_STATUS__c == "RENEWED",1,0)

  # splitting data into train and test
  smp_size <- floor(0.80 * nrow(new_data))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(new_data)), size = smp_size)

  train <- new_data[train_ind, ]
  test <- new_data[-train_ind, ]

  #logistic_output <- logistic_regression(train, test)
  #random_output <- forest_random(train, test)
  #extreme_output <- extereme_gradient(train, test)

  source("R/Model_Building.R")
  source("R/xg_boost.R")

  if (c > b) {
    test <- cbind(test, glm_predicted$predicted)
    print("Logistic Regression is Higher Accuracy")
    #source("R/Update_salesforce.R")
  } else {
    test <- cbind(test, data_predict$Predicted_data)
    print("Xtreme Gradient Boosting is Higher Accuracy")
    #source("R/Update_salesforce.R")
  }

  colnames(data1) <- c("Id", newname)

  updater(access_token, instance_url, object, data1)

  return(data1)






