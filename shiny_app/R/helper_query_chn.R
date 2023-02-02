#' @title helper: query chn data for review
#'
#' @description The \code{query_chn} function facilitates querying desert
#' fertilization chn tissue data from the database for review.
#'
#' @export

query_chn <- function() {

  parameterized_query <- glue::glue_sql('
    SELECT
      site_code,
      plot_id, 
      treatment_code,
      sample_date,
      season_year, 
      tissue_type,
      "Weight",
      "Comment", 
      "Carbon %",
      "Hydrogen %",
      "Nitrogen %"
    FROM urbancndep.plant_tissue_chn
    WHERE 
      sample_date >= { Sys.Date() - lubridate::years(5) } AND
      plot_id IS NOT NULL AND
      (
        "Comment" !~* \'need|require\' OR
        "Comment" IS NULL
      )
    ;
    ',
    .con = DBI::ANSI()
  )

  print(parameterized_query)
  run_interpolated_query(parameterized_query)

}
