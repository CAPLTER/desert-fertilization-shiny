#' @title module: cover events
#'
#' @description The module cover events facilitates entering Desert
#' Fertilization annuals cover data. This interface is written explicity as a
#' data-entry tool and purposefully omits more sophisiticated data querying and
#' viewing functionality.
#'
#' @export

# UI ---------------------------------------------------------------------------

cover_eventsUI <- function(id) {

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
            inputId = ns("add_new_cover_event"),
            label   = "add event",
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
          DT::dataTableOutput(ns("cover_events_view")),
          shiny::div(id = "add_cover_compositions")
        )

      )   # close the row
      ),  # close the page

    # js file and function within file respectively
    tags$script(src = "cover_events_module.js"),
    tags$script(paste0("cover_events_module_js('", ns(''), "')"))

  ) # close tagList

} # close UI


# main -------------------------------------------------------------------------

cover_events <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns # to facilitate renderUIs
    listener_init("update_cover_events")


    # cover events data ----------------------------------------------------------

    cover_events_reactive <- shiny::reactive({

      # add listener for adding records
      listener_watch("update_cover_events")

      cover_events_data <- query_cover_events()

      if (nrow(cover_events_data) == 0) {

        cover_events_data <- NULL

      } else {

        actions <- purrr::map_chr(cover_events_data$id, function(id_) {
          paste0(
            '<div class="btn-group" style="width: 120px;" role="group" aria-label="Basic example">
              <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
              <button class="btn btn-info btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="Observations" id = ', id_, ' style="margin-left: 5px;"><i class="fa fa-terminal"></i></button>
              <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin-left: 5px;"><i class="fa fa-trash-o"></i></button>
            </div>'
          )
        }
        )

        cover_events_data <- cbind(
          tibble::tibble("actions" = actions),
          cover_events_data
        )

      }

      return(cover_events_data)

    }) # close cover_events_reactive


    # render (non-editable) table of cover events data
    output$cover_events_view <- DT::renderDT({

      cover_events_reactive()

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
          targets   = c(9),
          render    = JS("$.fn.dataTable.render.ellipsis(30)")
        )
        ),
      bFilter       = FALSE,
      bLengthChange = FALSE,
      bPaginate     = FALSE,
      autoWidth     = FALSE
    )
    ) # close output$cover_events_view


    # delete cover events --------------------------------------------------------

    shiny::observeEvent(input$cover_event_to_delete, {

      parameterized_query <- glue::glue_sql('
        DELETE FROM urbancndep.cover_events
        WHERE cover_event_id = { as.numeric(input$cover_event_to_delete) }
        ;
        ',
        .con = DBI::ANSI()
      )

      run_interpolated_query(parameterized_query)

      listener_trigger("update_cover_events")

    })


    # edit existing cover event ------------------------------------------------

    this_cover_event_to_edit <- shiny::eventReactive(input$cover_event_to_edit, {

      this_cover_event <- cover_events_reactive() |>
      dplyr::filter(id == input$cover_event_to_edit) |>
      dplyr::select(-actions)

      return(this_cover_event)

    })

    edit_counter <- shiny::reactiveVal(value = 0)

    shiny::observeEvent(input$cover_event_to_edit, {

      id <- edit_counter()

      module_cover_event_new(
        id            = paste0("edit_cover_event", id),
        modal_title   = "edit cover event",
        ce_to_edit    = this_cover_event_to_edit
      )

      edit_counter(id + 1) # increment module counter

      if (edit_counter() > 1) {

        remove_shiny_inputs(ns(paste0("edit_cover_event", id)), input)
        remove_shiny_inputs(ns(paste0("edit_cover_event", id, "-edit_cover_event", id)), input)

      }

    },
    once       = FALSE,
    ignoreInit = TRUE
    )


    # add new cover event ------------------------------------------------------

    add_counter <- shiny::reactiveVal(value = 0)

    shiny::observeEvent(input$add_new_cover_event, {

      id <- add_counter()

      module_cover_event_new(
        id            = paste0("add_cover_event", id),
        modal_title   = "add cover event",
        ce_to_edit    = function() NULL
      )

      add_counter(id + 1) # increment module counter

      if (add_counter() > 1) {

        remove_shiny_inputs(ns(paste0("add_cover_event", id)), input)
        remove_shiny_inputs(ns(paste0("add_cover_event", id, "-add_cover_event", id)), input)

      }

    },
    once       = FALSE,
    ignoreInit = TRUE
    )


    # add observations to cover event ------------------------------------------

    this_ce_to_populate <- shiny::eventReactive(input$cover_event_to_populate, {

      this_cover_event <- cover_events_reactive() |>
      dplyr::filter(id == input$cover_event_to_populate) |>
      dplyr::select(-actions)

      return(this_cover_event)

    })


    populate_counter <- shiny::reactiveVal(value = 0)

    shiny::observeEvent(input$cover_event_to_populate, {

      id            <- populate_counter()
      ce_element_id <- paste0("ce_element_", id)

      shiny::insertUI(
        selector = "#add_cover_compositions",
        where    = "afterBegin",
        ui       = tags$div(
          id = ce_element_id,
          cover_compositionUI(ns(paste0("cover_composition_inventory", id)))
        )
      )

      cover_composition(
        id             = paste0("cover_composition_inventory", id),
        ce_to_populate = this_ce_to_populate
      )

      # increment module counter
      populate_counter(populate_counter() + 1)

      if (populate_counter() > 1) {

        shiny::removeUI(selector = paste0("#", ce_element_id))
        remove_shiny_inputs(ns(paste0("cover_composition_inventory", id)), input)
        remove_shiny_inputs(ns(paste0("cover_composition_inventory", id, "-cover_composition_inventory", id)), input)

      }

    },
    once       = FALSE,
    ignoreInit = TRUE
    )


    # debugging: module level --------------------------------------------------

    # observe(print({ this_ce_to_populate() }))
    # observe(print({ queryType$default }))
    # observe(print({ input$new_cover_event_collector }))


  }) # close module server
} # close module function
