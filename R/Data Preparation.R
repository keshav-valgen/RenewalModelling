

##################### Packages are reqiured ######################
library(dplyr)
library(sqldf)
library(Metrics)

###### Load the data ########################################
data2 = read.csv("C:/Users/Sudhakar/Desktop/Renewal Mdelling/2.R.Data.Extraction.csv")

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




########## Finding summary on the dependet variables
no_of_Accounts = sqldf("select RENEWAL_STATUS__C,count(RENEWAL_STATUS__C) as No_Of_Accts
                       ,sum(NET_UNITS__c) as No_Of_units
                       ,count(ORDER_NUMBER__c) as no_of_orders from data2 group by RENEWAL_STATUS__c")
# no_of_units = sqldf("select RENEWAL_STATUS__C,sum(NET_UNITS__c) as No_Of_units from data2 group by RENEWAL_STATUS__C")
# no_of_orders = sqldf("select RENEWAL_STATUS__C,sum(NET_UNITS__c) as Of_units from data2 group by RENEWAL_STATUS__C")



#### Creating RFMP Derived variables
prediction <-  function(Customer_id,
                        Order_id,
                        product_name,
                        Date_purchase,
                        Quantity,
                        Unit_Price,
                        Amount){
  #data <- data.frame(data2$Customer.Id,data2$Order_id,data2$Item_id,data2$Date, data2$Quantity,data2$Unit.Price, data2$Amount)

  data = data.frame(Customer_id, Order_id,product_name, Date_purchase, Quantity, Unit_Price, Amount)
  names(data)[1] <- "Customer_id"
  names(data)[2] <- "Order_id"
  names(data)[3] <- "Item_id"
  names(data)[4] <- "Date"
  names(data)[5] <- "Quantity"
  names(data)[6] <- "Unit_Price"
  names(data)[7] <- "Amount"
  #recency <- function(Customer_ID, Order_ID, Order_item, Unit_Price)
  data[,4] = as.Date(as.character(data$Date), format="%Y-%m-%d")
  data = na.omit(data)
  data$Ma_Date = max(data$Date)

  #Second_Max = max(Date[Date != max(Date)])
  data <- data %>% group_by(Customer_id) %>% mutate( Total_no_of_orders = length(unique(Order_id)),
                                                     Group_Max = max(Date),Second_Max = max(Date[Date != max(Date)]),
                                                     Recency_of_last_Order = Ma_Date - Group_Max,
                                                     Recency_of_Second_recent_Order = ifelse(is.na(as.Date(as.character(Second_Max), format="%Y-%m-%d")),0, (Ma_Date - Second_Max)),
                                                     Third_Max =  max(Date[Date != max(Date) & Date != Second_Max]),
                                                     Recency_of_Third_recent_Order = ifelse(is.na(as.Date(as.character(Third_Max), format="%Y-%m-%d")),0, (Ma_Date - Third_Max)),
                                                     Fourth_Max =  max(Date[Date != max(Date) & Date != Second_Max & Date != Third_Max]),
                                                     Fifth_Max =  max(Date[Date != max(Date) & Date != Second_Max & Date != Third_Max & Date != Fourth_Max]))





  # Handling missing values
  data$Date = ifelse(is.na(as.Date(as.character(data$Date),format = "%Y-%m-%d")),0,data$Date)
  data$Second_Max <- ifelse(is.na(as.Date(as.character(data$Second_Max), format="%Y-%m-%d")),0,data$Second_Max)
  data$Third_Max <- ifelse(is.na(as.Date(as.character(data$Third_Max), format="%Y-%m-%d")),0,data$Third_Max)
  data$Fourth_Max <- ifelse(is.na(as.Date(as.character(data$Fourth_Max), format="%Y-%m-%d")),0,data$Fourth_Max)
  data$Fifth_Max <- ifelse(is.na(as.Date(as.character(data$Fifth_Max), format="%Y-%m-%d")),0,data$Fifth_Max)
  data$Group_Max <- ifelse(is.na(as.Date(as.character(data$Group_Max), format="%Y-%m-%d")),0,data$Group_Max)

  # Difference in Days, between successive recent order dates
  data$RC_L3_L2_int_days <- data$Third_Max - data$Second_Max
  data$RC_L3_L2_int_days <- ifelse(data$RC_L3_L2_int_days < -365, 0 , data$RC_L3_L2_int_days)
  data$RC_L4_L3_int_days <- data$Fourth_Max - data$Third_Max
  data$RC_L4_L3_int_days <- ifelse(data$RC_L4_L3_int_days < -365, 0 , data$RC_L4_L3_int_days)
  data$RC_L5_L4_int_days <- data$Fifth_Max - data$Fourth_Max
  data$RC_L5_L4_int_days <- ifelse(data$RC_L5_L4_int_days < -365, 0 , data$RC_L5_L4_int_days)

  # Difference in days from the 2nd most recent order
  data$RC_L5_L2_int_days <- ifelse(data$Second_Max == 0 | data$Fifth_Max == 0,0,data$Fifth_Max - data$Second_Max)

  # Average interval days from the 2nd most recent order
  #data$RC_L2-L3_avg_days <-
  data$RC_L2_L4_avg_days <- (data$RC_L3_L2_int_days + data$RC_L4_L3_int_days) / 2
  data$RC_L2_L5_avg_days <- (data$RC_L3_L2_int_days + data$RC_L4_L3_int_days + data$RC_L5_L4_int_days) / 3

  # High-Low Dates
  data$RC_Hi_2_5_int_days <- min(data$RC_L3_L2_int_days, data$RC_L4_L3_int_days,data$RC_L5_L4_int_days)
  data$RC_Lo_2_5_int_days <- max(data$RC_L3_L2_int_days, data$RC_L4_L3_int_days,data$RC_L5_L4_int_days)


  # Finding total recent order amount
  data1 <- sqldf('select Customer_id,sum(Amount) as Recent_Order_Amount,MAX (Date) as Max_date from data group by Customer_id,Order_id', method = "name__class")
  data1 = sqldf("select *,Max(Max_date) from data1 group by Customer_id")
  data <-  merge(data,data1[, c(1,2)], by = "Customer_id", all.x = T)

  # Frequency  Orders in "x" time period
  # data  %>%
  #   filter(Date >= Group_Max - 30) %>%
  #   select(unique.Order_id=Order_id) %>%
  #   group_by(Customer_id) %>%
  #   summarise(count = n())
  data1 = sqldf('select Customer_id,count(DISTINCT Order_id) as Number_of_Orders_last_30_days from data where Date >= Ma_Date - 30 group by Customer_id', method = "name__class")
  data <- merge(data,data1[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)
  data$Number_of_Orders_last_30_days[is.na(data$Number_of_Orders_last_30_days)] = 0
  data1 = sqldf('select Customer_id,count(DISTINCT Order_id) as Number_of_Orders_last_90_days from data where Date >= Ma_Date -90 group by Customer_id', method = "name__class")
  data <- merge(data,data1[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)
  data$Number_of_Orders_last_90_days[is.na(data$Number_of_Orders_last_90_days)] = 0
  data1 = sqldf('select Customer_id,count(DISTINCT Order_id) as Number_of_Orders_last_180_days from data where Date >= Ma_Date - 180 group by Customer_id', method = "name__class")
  data <- merge(data,data1[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)
  data$Number_of_Orders_last_180_days[is.na(data$Number_of_Orders_last_180_days)] = 0
  data1 = sqldf('select Customer_id,count(DISTINCT Order_id) as Number_of_Orders_last_365_days from data where Date >= Ma_Date - 365 group by Customer_id', method = "name__class")
  data <- merge(data,data1[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)
  data$Number_of_Orders_last_365_days[is.na(data$Number_of_Orders_last_365_days)] = 0

  #  Distinct Products in "x" orders
  data3 = sqldf("select Customer_id, count(DISTINCT Item_id) as No_Of_Distinct_Products from data where Date == Second_Max group by Customer_id",method = "name__class")
  data <- merge(data,data3[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)
  data3 = sqldf("select Customer_id, count(DISTINCT Item_id) as No_Of_Distinct_Products from data where Date == Third_Max group by Customer_id",method = "name__class")
  data <- merge(data,data3[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)

  #  Distinct Product in "x" time period
  data1 = sqldf('select Customer_id,count(DISTINCT Item_id) as Number_of_items_last_30_days from data where Date >= Ma_Date - 30 group by Customer_id', method = "name__class")
  data <- merge(data,data1[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)
  data1 = sqldf('select Customer_id,count(DISTINCT Item_id) as Number_of_items_last_90_days from data where Date >= Ma_Date -90 group by Customer_id', method = "name__class")
  data <- merge(data,data1[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)
  data1 = sqldf('select Customer_id,count(DISTINCT Item_id) as Number_of_items_last_180_days from data where Date >= Ma_Date - 180 group by Customer_id', method = "name__class")
  data <- merge(data,data1[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)
  data1 = sqldf('select Customer_id,count(DISTINCT Item_id) as Number_of_items_last_365_days from data where Date >= Ma_Date - 365 group by Customer_id', method = "name__class")
  data <- merge(data,data1[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)


  #  Non-Order activity in "x" time period


  #  Revenues in "x" time period
  data1 = sqldf('select Customer_id,SUM(Amount) as Revenue_from_orders_of_last_30_days from data where Date >= Ma_Date - 30 group by Customer_id', method = "name__class")
  data <- merge(data,data1[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)

  data1 = sqldf('select Customer_id,SUM(Amount) as Revenue_from_orders_of_last_90_days from data where Date >= Ma_Date -90 group by Customer_id', method = "name__class")
  data <- merge(data,data1[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)
  data1 = sqldf('select Customer_id,SUM(Amount) as Revenue_from_orders_of_last_180_days from data where Date >= Ma_Date - 180 group by Customer_id', method = "name__class")
  data <- merge(data,data1[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)
  data1 = sqldf('select Customer_id,SUM(Amount) as Revenue_from_orders_of_last_365_days from data where Date >= Ma_Date - 365 group by Customer_id', method = "name__class")
  data <- merge(data,data1[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)


  #  Revenue in last "x" orders, Revenue from 2nd, 3rd and 4th recent order
  data3 = sqldf("select Customer_id, SUM(Amount) as Revenue_from_2nd_Order from data where Date == Second_Max group by Customer_id",method = "name__class")
  data <- merge(data,data3[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)
  data3 = sqldf("select Customer_id, SUM(Amount) as Revenue_from_3rd_Order from data where Date == Third_Max group by Customer_id",method = "name__class")
  data <- merge(data,data3[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)
  data3 = sqldf("select Customer_id, SUM(Amount) as Revenue_from_4th_Order from data where Date == Fourth_Max group by Customer_id",method = "name__class")
  data <- merge(data,data3[, c(1,2)], by = "Customer_id", all.x = T, all.y = T)

  # handling Missing Values
  for (i in 1:ncol(data)) {
    data[,i] = ifelse(is.na(data[,i]) == T,0, data[,i])
  }
  #  Average revenue in last "x" orders
  #data$MT_L2_L3_avg_dols <- (sum(data$Revenue_from_2nd_Order, data$Revenue_from_3rd_Order) / 2)
  #data$MT_L2_L4_avg_dols <- sum(data$Revenue_from_2nd_Order, data$Revenue_from_3rd_Order, data$Revenue_from_4th_Order) / 3



  data <- data %>% group_by(Customer_id) %>% mutate( avg_of_second_Third_order = sum(unique(Revenue_from_2nd_Order),unique(Revenue_from_3rd_Order))/2,
                                                     avg_of_Third_Fourth_order = sum(unique(Revenue_from_3rd_Order),unique(Revenue_from_4th_Order))/2)

  # Building Product derived variables
  #a <- data %>% group_by(Customer_id, Item_id) %>% summarise(total_value = sum(Amount)) %>% mutate()

  #a = sqldf("select *,SUM(Amount) as Total_Amount from data group by  Customer_id, Item_id")
  library(dplyr)
  library(tidyr)
  data <- data  %>% group_by(Customer_id) %>% mutate(Revenue_of_Category1 = max(Amount),
                                                     Revenue_of_Category2= max(Amount[Amount != max(Amount)]),
                                                     Revenue_of_Category3 =  max(Amount[Amount != max(Amount) & Amount != Revenue_of_Category2]),
                                                     Other_Category = sum(Amount[Amount != Revenue_of_Category1 & Amount != Revenue_of_Category2 & Amount != Revenue_of_Category3]))


  # handling Missing Values
  for (i in 1:ncol(data)) {
    data[,i] = ifelse(is.na(data[,i]) == T,0, data[,i])
  }

  # Replace Infinity values by zero
  data <- do.call(data.frame,lapply(data, function(x) replace(x, is.infinite(x),0)))


  # Selecting variables from the dataset
  #names(data)
  input_data = data[,c( "Quantity" , "Unit_Price" ,
                        "Amount",
                        "Total_no_of_orders",
                        "Second_Max", "Recency_of_last_Order",
                        "Recency_of_Second_recent_Order",
                        "Recency_of_Third_recent_Order" ,       "Fourth_Max"       ,
                        "Fifth_Max"  ,                          "RC_L3_L2_int_days" ,
                        "RC_L4_L3_int_days"  ,                  "RC_L5_L4_int_days"  ,
                        "RC_L5_L2_int_days" ,                   "RC_L2_L4_avg_days"  ,
                        "RC_L2_L5_avg_days"  ,                  "RC_Hi_2_5_int_days"  ,
                        "RC_Lo_2_5_int_days" ,                  "Recent_Order_Amount" ,
                        "Number_of_Orders_last_30_days" ,       "Number_of_Orders_last_90_days",
                        "Number_of_Orders_last_180_days" ,      "Number_of_Orders_last_365_days"  ,
                        "No_Of_Distinct_Products.x"   ,         "No_Of_Distinct_Products.y"  ,
                        "Number_of_items_last_30_days"   ,      "Number_of_items_last_90_days"   ,
                        "Number_of_items_last_180_days"  ,      "Number_of_items_last_365_days"  ,
                        "Revenue_from_orders_of_last_30_days",  "Revenue_from_orders_of_last_90_days" ,
                        "Revenue_from_orders_of_last_180_days", "Revenue_from_orders_of_last_365_days",
                        "Revenue_from_2nd_Order"    ,           "Revenue_from_3rd_Order",
                        "Revenue_from_4th_Order"   ,            "avg_of_second_Third_order" ,
                        "avg_of_Third_Fourth_order"   ,         "Revenue_of_Category1"  ,
                        "Revenue_of_Category2"    ,             "Revenue_of_Category3"  ,
                        "Other_Category")]


}

#function(Customer_id,Order_id,Date_purchase,Quantity, Unit_Price,Amount
#output = prediction(data2$B2B_UniqueID, data2$Actual_Employee_Size_Location, data2$Primary_State, data2$Create.Date,data2$Actual_Sales_Volume_Location, data2$Corporate_Employee_Size.Location, data2$Number_of_Years_In_Business, 0.80)


output = prediction(data1$ACCT_ID__c,
                    data1$ORDER_TYPE__c,
                    data1$Name,
                    data1$ORDER_RENEWAL_DATE__c,
                    data1$NET_UNITS__c,
                    data1$PRICE_PER_UNIT__c,
                    data1$REVENUE__c)

##### Finding sparse value variables
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
new_data$RENEWAL_STATUS__c <- as.numeric(new_data$RENEWAL_STATUS__c)

# splitting data into train and test
smp_size <- floor(0.80 * nrow(new_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(new_data)), size = smp_size)

train <- new_data[train_ind, ]
test <- new_data[-train_ind, ]


# Building the Multi Classification Model


