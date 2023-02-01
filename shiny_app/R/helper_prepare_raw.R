#' @title helper: write RAW machine output to a temporary database table and
#' return the query to insert those data into the corresponding table that
#' houses the raw data for that machine.
#'
#' @description The function \code{prepare_raw} adds RAW machine output data to
#' a temporary table in the urbancndep schema and returns the appropriate query
#' to insert those data into the production table that houses the raw data for
#' that machine.
#'
#' @export

prepare_raw <- function(raw_reactive, analysis) {

  temp_raw <- raw_reactive

  ## add batch

  if (grepl("resin", analysis, ignore.case = TRUE)) {

    temp_raw <- prepare_resin_data(temp_raw)

    current_batch_query <- "SELECT MAX(upload_batch) AS max FROM urbancndep.resin;"
    current_batch       <- run_interpolated_query(current_batch_query)
    next_batch          <- as.integer(current_batch[["max"]]) + 1

    temp_raw[["upload_batch"]] <- next_batch

  }


  ## write temporary table: raw data

  remove_table(
    schema_name = "urbancndep",
    table_name  = "temp_raw"
  )

  DBI::dbWriteTable(
    conn      = this_pool,
    name      = c("urbancndep", "temp_raw"),
    value     = temp_raw,
    row.names = FALSE
  )


  ## build raw insert query

  insert_raw_query <- build_insert_raw_query(currentTab = analysis)


  ## return

  return(insert_raw_query)

}
