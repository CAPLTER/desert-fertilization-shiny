#' @title helper: write raw and_or results data from temporary tables to
#' machine-specific raw and results data tables, respectively.
#'
#' @description The function \code{upload_chemistry} calls sub functions to
#' generate appropriate sql statements to write raw and results (as relevant)
#' chemistry data from temp_raw and temp_results into the the machine-specific
#' raw data table (e.g., lachat) and results tables, respectively. Those
#' statements are then executed within a transaction.
#'
#' @export

upload_chemistry <- function(
  this_raw_reactive     = NULL,
  this_results_reactive = NULL,
  this_samples_metadata = NULL,
  this_analysis         = NULL
  ) {

  # prepare data

  if (!is.null(this_raw_reactive)) {

    insert_raw_query <- prepare_raw(
      raw_reactive = this_raw_reactive,
      analysis     = this_analysis
    )

  }

  if (!is.null(this_results_reactive)) {

    insert_results_query <- prepare_results(
      results_reactive = this_results_reactive,
      samples_metadata = this_samples_metadata,
      analysis         = this_analysis,
      is_nitrite       = this_is_nitrite
    )

  }

  # print(insert_raw_query)
  # print(insert_results_query)

  ## begin tryCatch - transaction

  tryCatch({

    poolWithTransaction(this_pool, function(conn) {

      if (!is.null(this_raw_reactive)) {

        DBI::dbExecute(
          conn,
          insert_raw_query
        )

      }

      if (!is.null(this_results_reactive)) {

        DBI::dbExecute(
          conn,
          insert_results_query
        )

      }

})

    shiny::showNotification(
      ui          = "successfully uploaded",
      duration    = NULL,
      closeButton = TRUE,
      type        = "message",
      action      = shiny::a(href = "javascript:location.reload();", "reload the page")
    )

  }, warning = function(warn) {

    shiny::showNotification(
      ui          = paste("there is a warning:  ", warn),
      duration    = NULL,
      closeButton = TRUE,
      type        = "warning"
    )

    print(paste("WARNING: ", warn))

  }, error = function(err) {

    shiny::showNotification(
      ui          = paste("there was an error:  ", err),
      duration    = NULL,
      closeButton = TRUE,
      type        = "error"
    )

    print(paste("ERROR: ", err))
    print("ROLLING BACK TRANSACTION")

  }) # close try catch

} # close upload_chemistry
