#' @title helper: add a new fertilizer application record
#'
#' @description Function to add a new record of fertilizer application
#'
#' @export
#'

add_fertilizer <- function(
  fertilizer_site,
  fertilizer_date,
  fertilizer_n,
  fertilizer_p,
  fertilizer_np
  ) {

  fertilizer_site_numeric <- desfert_sites[desfert_sites$code == fertilizer_site, ][["id"]]
  fertilizer_site_numeric <- as.integer(fertilizer_site_numeric)
  fertilizer_n            <- as.numeric(fertilizer_n)
  fertilizer_p            <- as.numeric(fertilizer_p)
  fertilizer_np           <- gsub("[\r\n]", "; ", fertilizer_np)
  fertilizer_np           <- gsub(",", ";", fertilizer_np)
  fertilizer_np           <- ifelse(fertilizer_np == "", "NA", fertilizer_np)

  parameterized_query <- glue::glue_sql("
    INSERT INTO urbancndep.fertilizer_applications
    (
      site_id,
      date,
      \"N\",
      \"P\",
      \"N_and_P\"
    )
    VALUES
    (
      { fertilizer_site_numeric },
      { fertilizer_date },
      { fertilizer_n },
      { fertilizer_p },
      NULLIF ({ fertilizer_np }, 'NA')::text
    )
    ;
    ",
    .con = DBI::ANSI()
  )

  run_interpolated_execution(parameterized_query)
  # print(parameterized_query)

}
