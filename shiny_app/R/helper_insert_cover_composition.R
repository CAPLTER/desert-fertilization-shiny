#' @title helper: add new or update annuals cover composition (cc) record
#'
#' @description Function to add a new or edit existing
#' urbancndep.cover_composition (cc) record
#'
#' @export

insert_new_or_update_cc <- function(
  cover_type_description,
  cover_amount,
  query_type,
  cover_event_id       = NULL,
  cover_composition_id = NULL
  ) {

  this_cover_type_id <- cover_types[cover_types$cover_type == cover_type_description, ][["cover_type_id"]]
  if (!is.null(cover_event_id))       { cover_event_id <- as.integer(cover_event_id) }
  if (!is.null(cover_composition_id)) { cover_event_id <- as.integer(cover_composition_id) }


  if (query_type == "insert") {

    parameterized_query <- glue::glue_sql("
      INSERT INTO urbancndep.cover_composition
      (
        cover_event_id,
        cover_type_id,
        cover_amt
      )
      VALUES
      (
        { cover_event_id },
        { this_cover_type_id },
        { cover_amount }
      )
      ;
      ",
      .con = DBI::ANSI()
    )

  } else if (query_type == "update") {

    parameterized_query <- glue::glue_sql("
      UPDATE urbancndep.cover_composition
      SET
        cover_type_id = { this_cover_type_id },
        cover_amt     = { cover_amount }
      WHERE cover_id = { cover_composition_id }
      ;
      ",
      .con = DBI::ANSI()
    )

  } else {

    parameterized_query <- NULL
    stop("could not detect cover composition query type")

  }

  # print(parameterized_query)
  run_interpolated_execution(parameterized_query)

}
