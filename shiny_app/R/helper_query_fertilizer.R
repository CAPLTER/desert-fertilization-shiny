#' @title helper: query fertilizer data for review
#'
#' @description The \code{query_fertilizer} function facilitates querying
#' desert fertilization fertilizer application data from the database for
#' review.
#'
#' @export

query_fertilizer <- function() {

  parameterized_query <- glue::glue_sql('
    SELECT
      s.code AS site_code,
      f.date AS application_date,
      f."N" AS nitrogen,
      f."P" AS phosphorus,
      f."N_and_P" AS nit_and_phos
    FROM urbancndep.fertilizer_applications f
    JOIN urbancndep.sites s ON (f.site_id = s.id)
    WHERE
      f.date >= { Sys.Date() - lubridate::years(5) }
    ORDER BY
      f.id DESC
    ;
    ',
    .con = DBI::ANSI()
  )

  run_interpolated_query(parameterized_query)

}
