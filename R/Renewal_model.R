library(RForcecom)



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
                    Order_id,', ',
                    product_name,', ',
                    Date_purchase,', ',
                    Quantity,', ',
                    Unit_Price,', ',
                    Amount,', ',Others,' FROM ', object)


  session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)
  data1 <- rforcecom.bulkQuery(session, myquery, object)



