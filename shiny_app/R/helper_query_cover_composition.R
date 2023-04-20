#' @title helper: query (annual) cover compositions or composition details
#'
#' @description \code{query_cover_composition} facilitates querying desert
#' fertilization annual cover composition data for a given cover event; whereas
#' \code{query_cover_composition} facilitates querying details of a particular
#' composition record.
#'
#' @export

query_cover_compositions <- function(cover_event_id) {

  cover_event_id <- as.integer(cover_event_id)

  parameterized_query <- glue::glue_sql("
    SELECT
      cover_composition.cover_id AS id,
      cover_events.cover_event_id,
      cover_types.cover_type,
      cover_types.cover_category,
      cover_composition.cover_amt as amount
    FROM urbancndep.cover_composition
    JOIN urbancndep.cover_events ON (cover_composition.cover_event_id = cover_events.cover_event_id)
    JOIN urbancndep.cover_types ON (cover_composition.cover_type_id = cover_types.cover_type_id)
    WHERE
      cover_composition.cover_event_id = { cover_event_id }
    ORDER BY
      cover_events.cover_event_id,
      cover_types.cover_category DESC,
      cover_types.cover_type
    ;
    ",
    .con = DBI::ANSI()
  )

  # print(parameterized_query)
  run_interpolated_query(parameterized_query)

}


query_cover_composition <- function(cover_id) {

  cover_id <- as.integer(cover_id)

  parameterized_query <- glue::glue_sql("
    SELECT
      cover_composition.cover_id AS id,
      cover_events.cover_event_id,
      cover_types.cover_type,
      cover_types.cover_category,
      cover_composition.cover_amt as amount
    FROM urbancndep.cover_composition
    JOIN urbancndep.cover_events ON (cover_composition.cover_event_id = cover_events.cover_event_id)
    JOIN urbancndep.cover_types ON (cover_composition.cover_type_id = cover_types.cover_type_id)
    WHERE
      cover_composition.cover_id = { cover_id }
    ;
    ",
    .con = DBI::ANSI()
  )

  # print(parameterized_query)
  run_interpolated_query(parameterized_query)

}
