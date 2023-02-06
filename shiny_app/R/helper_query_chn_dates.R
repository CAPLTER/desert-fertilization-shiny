#' @title helper: query stem or annual biomass field sampling dates
#'
#' @description The functions \code{query_stems_dates} and
#' \code{query_stems_dates} facilitate querying the dates of stem (post) and
#' annuals biomass collections. These dates correspond to dates for which
#' leaves or annuals biomass would be collected for analysis of CHN in plant
#' tissue. These dates are then attached to imported CHN data.
#'
#' @export

query_stems_dates <- function(
  survey_year,
  survey_season
  ) {

  if (grepl("spring", survey_season, ignore.case = TRUE)) {

    survey_months <- c(4, 5, 6)

  } else {

    survey_months <- c(9, 10, 11)

  }

  survey_year <- as.integer(survey_year)

  parameterized_query <- glue::glue_sql("
    SELECT
      shrubs.plot_id,
      max(stems.post_date) AS collection_date
    FROM urbancndep.stems
    JOIN urbancndep.shrubs ON stems.shrub_id = shrubs.id
    WHERE
      EXTRACT (YEAR FROM stems.post_date) = { survey_year } AND
      EXTRACT (MONTH FROM stems.post_date) IN ({ survey_months* }) AND
      stems.post_date IS NOT NULL
    GROUP BY
      shrubs.plot_id
    ;
    ",
    .con = DBI::ANSI()
  )

  # print(parameterized_query)
  run_interpolated_query(parameterized_query)

}


query_annuals_dates <- function(survey_year) {

  survey_year <- as.integer(survey_year)

  parameterized_query <- glue::glue_sql("
    SELECT
      plot_id,
      MAX(\"date\") AS collection_date
    FROM
      urbancndep.annuals_biomass
    WHERE 
      EXTRACT (\"year\" from \"date\") = { survey_year }
    GROUP BY
      plot_id 
      ;
      ",
        .con = DBI::ANSI()
      )

      # print(parameterized_query)
      run_interpolated_query(parameterized_query)

}
