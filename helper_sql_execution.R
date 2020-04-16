#' @title Helpers to facilitate sql executions
#'
#' @description The functions run_interpolated_query() and
#'   run_interpolated_execution() are (sub)helper functions that establish and
#'   close database connections to facilitate either a SQL query to return an
#'   objet or execute a statement.
#'
#'   
run_interpolated_query <- function(interpolatedQuery) {
  
  tryCatch({
    
    queryResult <- dbGetQuery(desfertPool,
                              interpolatedQuery)
    
    return(queryResult)
    
  }, warning = function(warn) {
    
    showNotification(ui = paste("there is a warning:  ", warn),
                     duration = NULL,
                     closeButton = TRUE,
                     type = 'warning')
    
    print(paste("WARNING: ", warn))
    
  }, error = function(err) {
    
    showNotification(ui = paste("there was an error:  ", err),
                     duration = NULL,
                     closeButton = TRUE,
                     type = 'error')
    
    print(paste("ERROR: ", err))
    print("ROLLING BACK TRANSACTION")
    
  }) # close try catch
  
} # close run_interpolated_query 


run_interpolated_execution <- function(interpolatedQuery) {
  
  tryCatch({
    
    poolWithTransaction(desfertPool, function(conn) {
      dbExecute(conn,
                interpolatedQuery)
    })    
    
    showNotification(ui = "success",
                     duration = 5,
                     closeButton = TRUE,
                     type = 'message')
    
  }, warning = function(warn) {
    
    showNotification(ui = paste("there is a warning:  ", warn),
                     duration = NULL,
                     closeButton = TRUE,
                     type = 'warning')
    
    print(paste("WARNING: ", warn))
    
  }, error = function(err) {
    
    showNotification(ui = paste("there was an error:  ", err),
                     duration = NULL,
                     closeButton = TRUE,
                     type = 'error')
    
    print(paste("ERROR: ", err))
    print("ROLLING BACK TRANSACTION")
    
  }) # close try catch
  
} # close run_interpolated_execution 


