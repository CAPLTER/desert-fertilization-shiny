#' @title helper: delete an existing cover composition record
#'
#' @description Function to delete an existing record of
#' urbancndep.cover_composition.

delete_cover_composition <- function(cc_id) {

  cc_id <- as.integer(cc_id)

  base_query <- glue::glue_sql("
    DELETE FROM urbancndep.cover_composition
    WHERE cover_id = { cc_id }
    ;
    ",
    .con = DBI::ANSI()
  )

  # print(base_query)
  run_interpolated_execution(base_query)

}
