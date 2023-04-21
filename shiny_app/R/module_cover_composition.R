#' @title module: cover composition
#'
#' @description The module cover composition facilitates adding, deleting, and
#' editing Desert Fertilization cover composition data.
#'
#' @note In other, similar applications, deleting an observation is wrapped in
#' a \code{tryCatch} even though \code{tryCatch} is a part of
#' \code{run_interpolated_execution}. I cannot recall why that was done in
#' other circumstances. It was not implemented here and seems to work fine.
#'
#' @export

# cover composition UI ---------------------------------------------------------

cover_compositionUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(
      id = "row_reference_cover_event_details",
      shiny::hr(),

      shiny::column(
        id     = "column_reference_cover_event_details",
        width  = 12,
        offset = 0,
        shiny::verbatimTextOutput(ns("cover_event_details_view"))
      ) # close column

      ), # close row

    shiny::fluidRow(
      id = "row_cover_composition",

      shiny::column(
        id     = "column_buttons",
        width  = 1,
        offset = 0,
        style = "padding-left: 30px; padding-right: 10px;",
        shiny::br(),
        shiny::br(),
        shiny::actionButton(
          inputId = ns("add_cover_composition"),
          label   = "add cover",
          class   = "btn-success",
          style   = "color: #fff; margin-bottom: 2px;",
          icon    = shiny::icon("plus"),
          width   = "100%"
        )
        ),

      shiny::column(
        id     = "column_data",
        width  = 10,
        offset = 0,
        style  = "padding-right: 30px;",
        DT::DTOutput(ns("cover_composition_view"))
      )  # close column
      ), # close row

    # js file and function within file respectively
    tags$script(src = "cover_composition_module.js"),
    tags$script(paste0("cover_composition_module_js('", ns(''), "')"))

  ) # close tagList

} # close cover_compositionUI


# cover composition ------------------------------------------------------------

cover_composition <- function(id, ce_to_populate) {

  shiny::moduleServer(id, function(input, output, session) {

    # added to facilitate renderUIs
    ns <- session$ns

    # query cover event details for reference
    cover_event_details_reactive <- shiny::reactive({

      cover_event_details <- query_cover_event(
        cover_event_id = ce_to_populate()$id
        )

      return(cover_event_details)

    })

    output$cover_event_details_view <- shiny::renderText({

      paste0(
        "site: ",       cover_event_details_reactive()[["site"]],       " | ",
        "plot: ",       cover_event_details_reactive()[["plot"]],       " | ",
        "treatment: ",  cover_event_details_reactive()[["treatment"]],  " | ",
        "position: ",   cover_event_details_reactive()[["position"]],   " | ",
        "subplot: ",    cover_event_details_reactive()[["subplot"]],    " | ",
        "date: ",       cover_event_details_reactive()[["date"]]
      )

    })



    cover_compositions_reactive <- shiny::reactive({

      listener_watch("update_composition") 

      cover_compositions_queried <- query_cover_compositions(
        cover_event_id = ce_to_populate()$id
      )

      if (nrow(cover_compositions_queried) == 0) {

        cover_compositions_queried <- NULL

      } else {

        actions <- purrr::map_chr(cover_compositions_queried$id, function(id_) {
          paste0(
            '<div class="btn-group" style="width: 120px;" role="group" aria-label="Basic example">
              <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
              <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin-left: 5px;"><i class="fa fa-trash-o"></i></button>
            </div>'
          )
        }
        )

        cover_compositions_queried <- cbind(
          tibble::tibble("actions" = actions),
          cover_compositions_queried
        )

      }

      return(cover_compositions_queried)

    })


    output$cover_composition_view <- DT::renderDT({

      cover_compositions_reactive()

    },
    class     = "cell-border stripe",
    escape    = FALSE,
    selection = "none",
    rownames  = FALSE,
    options   = list(
      columnDefs = list(
        list(
          targets   = c(0),
          width     = "100px"
          ),
        # list(
        #   targets   = c(1),
        #   visible   = FALSE
        #   ),
        list(
          targets   = c(0),
          className = "dt-center"
        )
        ),
      bFilter       = FALSE,
      bLengthChange = FALSE,
      bPaginate     = FALSE,
      autoWidth     = FALSE
    )
    ) # close cover_composition_view


    # add new cover composition observation ----------------------------------------

    add_composition_counter <- shiny::reactiveVal(value = 0)

    shiny::observeEvent(input$add_cover_composition, {

      id <- add_composition_counter()

      module_cover_composition_new(
        id                  = paste0("add_composition", id),
        modal_title         = "add cover composition",
        ce_id               = ce_to_populate()$id,
        composition_to_edit = function() NULL
      )

      add_composition_counter(id + 1) # increment module counter

      if (add_composition_counter() > 1) {

        remove_shiny_inputs(ns(paste0("add_composition", id)), input)
        remove_shiny_inputs(ns(paste0("add_composition", id, "-add_composition", id)), input)

      }

    },
    once       = FALSE,
    ignoreInit = TRUE
    )


    # edit cover composition

    this_composition_to_edit <- shiny::eventReactive(input$cover_composition_to_edit, {

      this_composition <- query_cover_composition(cover_id = input$cover_composition_to_edit)

      return(this_composition)

    })

    edit_composition_counter <- shiny::reactiveVal(value = 0)

    shiny::observeEvent(input$cover_composition_to_edit, {

      id <- edit_composition_counter()

      module_cover_composition_new(
        id                  = paste0("edit_composition", id),
        modal_title         = "edit cover composition",
        composition_to_edit = this_composition_to_edit
      )

      edit_composition_counter(id + 1) # increment module counter

      if (edit_composition_counter() > 1) {

        remove_shiny_inputs(ns(paste0("edit_composition", id)), input)
        remove_shiny_inputs(ns(paste0("edit_composition", id, "-edit_composition", id)), input)

      }

    },
    once       = FALSE,
    ignoreInit = TRUE
    )


    # delete cover composition observation -------------------------------------

    shiny::observeEvent(input$cover_composition_to_delete, {

      delete_cover_composition(cc_id = input$cover_composition_to_delete)

      listener_trigger("update_composition")

    }) # close delete cover composition

    
    # debugging: module level --------------------------------------------------

    # print(head(taxa))
    # observe(print({ this_composition_to_edit() }))
    # observe(print({ tse_to_populate() }))
    # observe(print({ input$trap_specimen_id_to_edit }))

  }) # close module sever
} # close module function
