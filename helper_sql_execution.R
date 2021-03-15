#' @title Helpers to facilitate sql executions
#'
#' @description The functions run_interpolated_query() and
#'   run_interpolated_execution() are (sub)helper functions that establish and
#'   close database connections to facilitate either a SQL query to return an
#'   objet or execute a statement.
#'
#'
run_interpolated_query <- function(interpolatedQuery) {

  # establish db connection
  pg <- database_connection()

  # close db connection after function call exits
  on.exit(dbDisconnect(pg))

  tryCatch({

    queryResult <- dbGetQuery(
      pg,
      interpolatedQuery)

    return(queryResult)

  }, warning = function(warn) {

    showNotification(
      ui = paste("there is a warning:  ", warn),
      duration = NULL,
      closeButton = TRUE,
      type = "warning")

    print(paste("WARNING: ", warn))

  }, error = function(err) {

    showNotification(
      ui = paste("there was an error:  ", err),
      duration = NULL,
      closeButton = TRUE,
      type = "error")

    print(paste("ERROR: ", err))
    print("transaction not executed")

  }) # close try catch

  # close database connection
  dbDisconnect(pg)

} # close run_interpolated_query


run_interpolated_execution <- function(interpolatedQuery) {

  # establish db connection
  pg <- database_connection()

  # close db connection after function call exits
  on.exit(dbDisconnect(pg))

  tryCatch({

    dbGetQuery(pg, "BEGIN TRANSACTION")

    # execute query
    dbExecute(
      pg,
      interpolatedQuery)

    dbCommit(pg)

    showNotification(
      ui = "action committed",
      duration = 3,
      closeButton = TRUE,
      type = "message")

  }, warning = function(warn) {

    showNotification(
      ui = paste("there is a warning:  ", warn),
      duration = NULL,
      closeButton = TRUE,
      type = "warning")

    print(paste("WARNING: ", warn))

  }, error = function(err) {

    showNotification(
      ui = paste("there was an error:  ", err),
      duration = NULL,
      closeButton = TRUE,
      type = "error")

    print(paste("ERROR: ", err))
    print("ROLLING BACK TRANSACTION")

    dbRollback(pg)

  }) # close try catch

  # close database connection
  dbDisconnect(pg)

} # close run_interpolated_execution
