library(RForcecom)
username <- "sudhakar@valgen.com"
password <- "Mari@9164385223yW3h7K7eR80gwtS1LJDGTnSn" #password+securitytoken
instanceURL <- "https://pvanalytics-dev-ed.my.salesforce.com/"
apiVersion <- "26.0"
session <- rforcecom.login(username, password, instanceURL, apiVersion)

objectName <- "Leads"
fields <- c("Id","Address","Campaign")
a = rforcecom.retrieve(session, objectName, fields)


get_salesforce_query <- "select * from Opportunity"
query <- "select Account.Name, Id, Account.Id from Opportunity"
query <- "select Account.Name, Id from Opportunity where StageName='Prospecting'"

salesforce_merchants <- rforcecom.query(session, get_salesforce_query)

# query
objects <- rforcecom.getObjectList(session)

# pull all fields of an object
getAllFields <- function(objectName) {
  description <- rforcecom.getObjectDescription(session, objectName)
  fields <- as.character(description$name)
  rforcecom.retrieve(session, objectName, fields)
}

# grab the data
oppurtunity <- getAllFields("Opportunity")
