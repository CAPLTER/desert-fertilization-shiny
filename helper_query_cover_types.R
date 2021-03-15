#' @title helper: identify key values for cover events and types
#'
#' @description Generates lists of (1) all possible cover types that are in
#'  use, and (2) all unique sites and plots.


# cover types ------------------------------------------------------------------

query_cover_types <- function() {

  base_query <- "
  SELECT
    cover_types.cover_type_id,
    cover_types.cover_category,
    cover_types.cover_type
  FROM urbancndep.cover_types
  WHERE
  cover_types.cover_type_id NOT IN (
    SELECT
      cover_types.cover_type_id
    FROM urbancndep.cover_types
    WHERE
      cover_types.cover_type ~~ ANY('{%2017%, %2018%, %2019%, sampled}') AND
      cover_types.cover_type ~~* '%comparable%'
  )
  ORDER BY
    cover_types.cover_category,
    cover_types.cover_type
  ;
  "

  parameterized_query <- sqlInterpolate(
    ANSI(),
    base_query)

  run_interpolated_query(parameterized_query)

}

cover_types <- query_cover_types()


# unique sites and plots -------------------------------------------------------

query_cover_sites_plots <- function() {

  base_query <- "
  SELECT DISTINCT
    sites.id AS site_id,
    sites.code as site_code,
    cover_events.plot as plot_id
  FROM
    urbancndep.cover_events
  JOIN urbancndep.plots ON (cover_events.plot = plots.id)
  JOIN urbancndep.sites ON (sites.id = plots.site_id)
  WHERE
    cover_events.year >= 2019
  ORDER BY
    plot_id ;
  "

  parameterized_query <- sqlInterpolate(
    ANSI(),
    base_query)

  run_interpolated_query(parameterized_query)

}

cover_sites_plots  <- query_cover_sites_plots()
