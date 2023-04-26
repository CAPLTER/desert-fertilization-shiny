#' @title helper: add new or update annuals biomass (ab) record
#'
#' @description Function to add a new or edit existing
#' urbancndep.annuals_biomass (ab) record
#'
#' @export

insert_new_or_update_ab <- function(
  annual_biomass_plot,
  annual_biomass_lwp,
  annual_biomass_replicate,
  annual_biomass_orientation,
  annual_biomass_date,
  annual_biomass_mass,
  annual_biomass_notes,
  annual_biomass_quadrat,
  query_type,
  ab_id = NULL
  ) {

  annual_biomass_year     <- as.integer(lubridate::year(annual_biomass_date))
  annual_biomass_notes    <- gsub("[\r\n]", "; ", annual_biomass_notes)
  annual_biomass_notes    <- gsub(",", ";", annual_biomass_notes)
  annual_biomass_notes    <- ifelse(annual_biomass_notes == "", "NA", annual_biomass_notes)
  this_annuals_biomass_id <- as.integer(ab_id)

  if (query_type == "insert") {

    parameterized_query <- glue::glue_sql("
      INSERT INTO urbancndep.annuals_biomass
      (
        plot_id,
        location_within_plot,
        replicate,
        subquad_orientation,
        \"date\",
        \"year\",
        mass,
        notes,
        quadrat
      )
      VALUES(
        { annual_biomass_plot },
        { annual_biomass_lwp },
        { annual_biomass_replicate },
        { annual_biomass_orientation },
        { annual_biomass_date },
        { annual_biomass_year },
        { annual_biomass_mass },
        NULLIF ({ annual_biomass_notes }, 'NA')::text,
        { annual_biomass_quadrat }
      )
      ;
      ",
      .con = DBI::ANSI()
    )

  } else if (query_type == "update") {

    parameterized_query <- glue::glue_sql("
      UPDATE urbancndep.annuals_biomass
      SET
        plot_id               = { annual_biomass_plot },
        location_within_plot  = { annual_biomass_lwp },
        replicate             = { annual_biomass_replicate },
        subquad_orientation   = { annual_biomass_orientation },
        \"date\"              = { annual_biomass_date },
        \"year\"              = { annual_biomass_year },
        mass                  = { annual_biomass_mass },
        notes                 = NULLIF ({ annual_biomass_notes }, 'NA')::text,
        quadrat               = { annual_biomass_quadrat }
      WHERE ann_biomass_id = { this_annuals_biomass_id }
        ;
      ",
      .con = DBI::ANSI()
    )

  } else {

    parameterized_query <- NULL
    stop("could not detect annuals biomass query type")

  }

  run_interpolated_execution(parameterized_query)

}
