#' @title helper: query resin data for review
#'
#' @description The \code{query_resin} function facilitates querying desert
#' fertilization resin data from the database for review.
#'
#' @export

query_resin <- function() {

  parameterized_query <- glue::glue_sql("
    SELECT
      field_id,
      collection_date,
      notes,
      sample_id,
      replicate_number AS replicate,
      manual_dilution_factor AS MDF,
      auto_dilution_factor AS ADF,
      detection_date,
      detection_time,
      analyte_name,
      peak_concentration AS peak_conc,
      determined_conc,
      conc_x_adf,
      conc_x_mdf,
      conc_x_adf_x_mdf,
      omit,
      sourcefile
    FROM
      urbancndep.resin
    WHERE
      collection_date >= { Sys.Date() - lubridate::years(5) } AND
      sample_type ~~* 'unknown'
    ORDER BY
      upload_batch DESC,
      id ASC
    ;
    ",
    .con = DBI::ANSI()
  )

  run_interpolated_query(parameterized_query)

}
