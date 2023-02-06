#' @title helper: query chn data for review
#'
#' @description The \code{query_chn} function facilitates querying desert
#' fertilization chn tissue data from the database for review.
#'
#' @export

query_chn <- function() {

  parameterized_query <- glue::glue_sql('
    SELECT
      plot_id, 
      collection_date,
      tissue_type,
      "Weight",
      "Comment", 
      "Carbon %",
      "Hydrogen %",
      "Nitrogen %"
    FROM urbancndep.plant_tissue_chn
    WHERE 
      collection_date >= { Sys.Date() - lubridate::years(8) } AND
      plot_id IS NOT NULL AND
      omit = FALSE
    ORDER BY
      upload_batch DESC,
      id DESC
    ;
    ',
    .con = DBI::ANSI()
  )

  # print(parameterized_query)
  run_interpolated_query(parameterized_query)

}
