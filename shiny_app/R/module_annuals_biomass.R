#' @title module: annuals biomass
#'
#' @description The module annuals biomass facilitates entering Desert
#' Fertilization annuals biomass data. This interface is written explicity as a
#' data-entry tool and purposefully omits more sophisiticated data querying and
#' viewing functionality.
#'
#' @note This module calls \code{module_annual_biomass_new} to facilitate
#' adding and editing biomass data (overkill).
#'
#' @export

# UI ---------------------------------------------------------------------------

annuals_biomassUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidPage(

      shiny::fluidRow(
        align = "middle",

        shiny::column(
          id    = "readme_row",
          width = 10,
          align = "left",
          shiny::div(
            id = "readme_box",
            "table displays only data from the current year"
          )  # close readme div
          ), # close readme column
        shiny::column(
          id    = "readme_row",
          width = 2,
          shiny::br(),
          shiny::actionButton(
            inputId = ns("add_new_biomass"),
            label   = "add biomass",
            class   = "btn-success",
            style   = "color: #fff;",
            icon    = shiny::icon("plus"),
            width   = "100%"
          )
        )
        ),  # close readme row

      shiny::fluidRow(

        shiny::column(
          id    = "right_panel",
          width = 12,
          DT::dataTableOutput(ns("annuals_biomass_view"))
        )

      )   # close the row
      ),  # close the page

    # js file and function within file respectively
    tags$script(src = "annuals_biomass_module.js"),
    tags$script(paste0("annuals_biomass_module_js('", ns(''), "')"))

  ) # close tagList

} # close UI


# main -------------------------------------------------------------------------

annuals_biomass <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns # to facilitate renderUIs
    listener_init("update_annuals_biomass")


    # cover events data ----------------------------------------------------------

    annuals_biomass_reactive <- shiny::reactive({

      # add listener for adding records
      listener_watch("update_annuals_biomass")

      annuals_biomass_data <- query_annuals_biomass()

      if (nrow(annuals_biomass_data) == 0) {

        annuals_biomass_data <- NULL

      } else {

        actions <- purrr::map_chr(annuals_biomass_data$id, function(id_) {
          paste0(
            '<div class="btn-group" style="width: 100px;" role="group" aria-label="Basic example">
              <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
              <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin-left: 5px;"><i class="fa fa-trash-o"></i></button>
            </div>'
          )
      }
        )

        annuals_biomass_data <- cbind(
          tibble::tibble("actions" = actions),
          annuals_biomass_data
        )

      }

      return(annuals_biomass_data)

    }) # close annuals_biomass_reactive


    # render (non-editable) table of cover events data
    output$annuals_biomass_view <- DT::renderDT({

      annuals_biomass_reactive()

    },
    class     = c("compact" ,"cell-border stripe"),
    plugin    = c("ellipsis"),
    escape    = FALSE,
    selection = "none",
    rownames  = FALSE,
    options   = list(
      columnDefs = list(
        list(
          targets   = c(0),
          width     = "100px"
          ),
        list(
          targets   = c(0),
          className = "dt-center"
          ),
        list(
          targets   = c(10),
          render    = JS("$.fn.dataTable.render.ellipsis(25)")
        )
        ),
      bFilter       = FALSE,
      bLengthChange = FALSE,
      bPaginate     = FALSE,
      autoWidth     = FALSE
    )
    ) # close output$annuals_biomass_view


    # delete cover events --------------------------------------------------------

    shiny::observeEvent(input$annuals_biomass_to_delete, {

      parameterized_query <- glue::glue_sql('
        DELETE FROM urbancndep.annuals_biomass
        WHERE ann_biomass_id = { as.integer(input$annuals_biomass_to_delete) }
        ;
        ',
        .con = DBI::ANSI()
      )

      run_interpolated_execution(parameterized_query)

      listener_trigger("update_annuals_biomass")

    })


    # edit existing cover event ------------------------------------------------

    this_annuals_biomass_to_edit <- shiny::eventReactive(input$annuals_biomass_to_edit, {

      this_annual_biomass <- annuals_biomass_reactive() |>
      dplyr::filter(id == input$annuals_biomass_to_edit) |>
      dplyr::select(-actions)

      return(this_annual_biomass)

    })

    edit_counter <- shiny::reactiveVal(value = 0)

    shiny::observeEvent(input$annuals_biomass_to_edit, {

      id <- edit_counter()

      module_annual_biomass_new(
        id            = paste0("edit_annual_biomass", id),
        modal_title   = "edit annual biomass",
        ab_to_edit    = this_annuals_biomass_to_edit
      )

      edit_counter(id + 1) # increment module counter

      if (edit_counter() > 1) {

        remove_shiny_inputs(ns(paste0("edit_annual_biomass", id)), input)
        remove_shiny_inputs(ns(paste0("edit_annual_biomass", id, "-edit_annual_biomass", id)), input)

      }

    },
    once       = FALSE,
    ignoreInit = TRUE
    )


    # add new cover event ------------------------------------------------------

    add_counter <- shiny::reactiveVal(value = 0)

    shiny::observeEvent(input$add_new_biomass, {

      id <- add_counter()

      module_annual_biomass_new(
        id            = paste0("add_annual_biomass", id),
        modal_title   = "add annual biomass",
        ab_to_edit    = function() NULL
      )

      add_counter(id + 1) # increment module counter

      if (add_counter() > 1) {

        remove_shiny_inputs(ns(paste0("add_annual_biomass", id)), input)
        remove_shiny_inputs(ns(paste0("add_annual_biomass", id, "-add_annual_biomass", id)), input)

      }

    },
    once       = FALSE,
    ignoreInit = TRUE
    )


    # debugging: module level --------------------------------------------------

    # observe(print({ this_annuals_biomass_to_edit() }))
    # observe(print({ queryType$default }))
    # observe(print({ input$new_cover_event_collector }))


  }) # close module server
} # close module function
