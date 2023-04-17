#' @title helper: query (annual) cover events
#'
#' @description The \code{query_cover_events} function facilitates querying
#' desert fertilization annual cover event data from the database.
#'
#' @export

query_cover_events <- function() {

  parameterized_query <- glue::glue_sql('
    SELECT
      cover_events.cover_event_id AS id,
      sites.code AS site,
      cover_events.plot AS plot,
      treatments.code AS treatment,
      cover_events.patch_type AS position,
      cover_events.subplot,
      cover_events.sample_date AS date,
      cover_events.year,
      cover_events.collector
    FROM urbancndep.cover_events
    JOIN urbancndep.plots ON (cover_events.plot = plots.id)
    JOIN urbancndep.sites ON (sites.id = plots.site_id)
    JOIN urbancndep.treatments treatments ON (treatments.id = plots.treatment_id)
    WHERE
      cover_events.year >= { lubridate::year(Sys.Date()) }
    ORDER BY
      cover_events.cover_event_id DESC
    ;
    ',
    .con = DBI::ANSI()
  )

  run_interpolated_query(parameterized_query)

}
