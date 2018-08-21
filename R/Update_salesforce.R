#### Function to Update data back to Salesforce


updater <- function(access_token, instance_url, myobject, data){

  instance_u <- paste0(instance_url,'/')
  api <- '36.0'
  session <- c(sessionID = access_token,instanceURL = instance_u, apiVersion = api)

  update_job <- rforcecom.createBulkJob(session,
                                        operation ='update', object = myobject) # Create a new bulkjob for updating Salesforce
  my_data <- data # Dataframe to be uploaded
  batches_info <- rforcecom.createBulkBatch(session,
                                            jobId = update_job$id, data = my_data) #Update job
  close_job_info <- rforcecom.closeBulkJob(session, jobId=update_job$id)
}
