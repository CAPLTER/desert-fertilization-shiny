#' @title helper: query annuals biomass or annuals biomass details
#'
#' @description \code{query_annuals_biomass} function facilitates querying
#' desert fertilization annual biomass data from the database for a given date
#' range. \code{query_annual_biomass} queries annual biomass details for a
#' given annual biomass record.
#'
#' @export

query_annuals_biomass <- function() {

  parameterized_query <- glue::glue_sql('
    SELECT
      annuals_biomass.ann_biomass_id AS id,
      sites.code AS site,
      annuals_biomass.plot_id,
      annuals_biomass.location_within_plot,
      annuals_biomass.replicate,
      annuals_biomass.subquad_orientation,
      annuals_biomass."date",
      annuals_biomass."year",
      annuals_biomass.mass,
      annuals_biomass.notes,
      annuals_biomass.quadrat
    FROM urbancndep.annuals_biomass
    JOIN urbancndep.plots ON (plots.id = annuals_biomass.plot_id)
    JOIN urbancndep.sites ON (sites.id = plots.site_id)
    WHERE
      annuals_biomass."year" >= { lubridate::year(Sys.Date()) }
    ORDER BY
      annuals_biomass.ann_biomass_id DESC
    ;
    ',
    .con = DBI::ANSI()
  )

  run_interpolated_query(parameterized_query)

}


query_annual_biomass <- function(annual_biomass_id) {

  annual_biomass_id <- as.integer(annual_biomass_id)

  parameterized_query <- glue::glue_sql('
    SELECT
      annuals_biomass.ann_biomass_id AS id,
      sites.code AS site,
      annuals_biomass.plot_id,
      annuals_biomass.location_within_plot,
      annuals_biomass.replicate,
      annuals_biomass.subquad_orientation,
      annuals_biomass."date",
      annuals_biomass."year",
      annuals_biomass.mass,
      annuals_biomass.notes,
      annuals_biomass.quadrat
    FROM urbancndep.annuals_biomass
    JOIN urbancndep.plots ON (plots.id = annuals_biomass.plot_id)
    JOIN urbancndep.sites ON (sites.id = plots.site_id)
    WHERE
      annuals_biomass.ann_biomass_id = { annual_biomass_id }
    ;
    ',
    .con = DBI::ANSI()
  )

  run_interpolated_query(parameterized_query)

}
