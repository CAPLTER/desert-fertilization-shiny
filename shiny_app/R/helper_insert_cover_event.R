#' @title helper: add new or update annuals cover event (ce) record
#'
#' @description Function to add a new or edit existing urbancndep.cover_events
#' (ce) record
#'
#' @export

insert_new_or_update_ce <- function(
  cover_event_plot,
  cover_event_position,
  cover_event_subplot,
  cover_event_collector,
  cover_event_date,
  query_type,
  ce_id = NULL
  ) {

  cover_event_year    <- as.integer(lubridate::year(cover_event_date))
  this_cover_event_id <- as.integer(ce_id)

  if (query_type == "insert") {

    parameterized_query <- glue::glue_sql("
      INSERT INTO urbancndep.cover_events
      (
        sample_date,
        year,
        plot,
        patch_type,
        subplot,
        collector
      )
      VALUES
      (
        { cover_event_date },
        { cover_event_year },
        { cover_event_plot },
        { cover_event_position },
        { cover_event_subplot },
        { cover_event_collector }
      )
      ;
      ",
      .con = DBI::ANSI()
    )

  } else if (query_type == "update") {

    parameterized_query <- glue::glue_sql("
      UPDATE urbancndep.cover_events
      SET
        sample_date = { cover_event_date },
        \"year\"    = { cover_event_year },
        plot        = { cover_event_plot },
        patch_type  = { cover_event_position },
        subplot     = { cover_event_subplot },
        collector   = { cover_event_collector }
      WHERE cover_event_id = { this_cover_event_id }
      ;
      ",
      .con = DBI::ANSI()
    )

  } else {

    parameterized_query <- NULL
    stop("could not detect cover event query type")

  }

  run_interpolated_execution(parameterized_query)

}


# add_cover_event <- function(
#   cover_event_plot,
#   cover_event_position,
#   cover_event_subplot,
#   cover_event_collector,
#   cover_event_date
#   ) {

#   cover_event_year <- as.integer(lubridate::year(cover_event_date))

#   parameterized_query <- glue::glue_sql("
#     INSERT INTO urbancndep.cover_events
#     (
#       sample_date,
#       year,
#       plot,
#       patch_type,
#       subplot,
#       collector
#     )
#     VALUES
#     (
#       { cover_event_date },
#       { cover_event_year },
#       { cover_event_plot },
#       { cover_event_position },
#       { cover_event_subplot },
#       { cover_event_collector }
#     )
#     ;
#     ",
#     .con = DBI::ANSI()
#   )

#   # print(parameterized_query)
#   run_interpolated_execution(parameterized_query)

# }
