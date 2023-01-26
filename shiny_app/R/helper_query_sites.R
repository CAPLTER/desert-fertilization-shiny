#' @title helper: query site data for SQL functions
#'
#' @description The \code{query_resin} function facilitates querying desert
#' fertilization resin data from the database for review.
#'
#' @export

query_sites <- function() {

  parameterized_query <- glue::glue_sql('
    SELECT
      id,
      code,
      "name",
      plots_description,
      region,
      utm_e,
      utm_n
    FROM urbancndep.sites
    WHERE
      plots_description IS NOT NULL
    ;
    ',
    .con = DBI::ANSI()
  )

  run_interpolated_query(parameterized_query)

}

desfert_sites <- query_sites()
