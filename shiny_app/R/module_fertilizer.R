#' @title module: fertilizer application history
#'
#' @description The fertilizer module facilitates viewing, searching, and
#' adding fertilizer records.
#'
#' @export
#'

# fertilizer UI ----------------------------------------------------------------

fertilizerUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidPage(

      shiny::fluidRow(

        shiny::column(
          id = ns("leftPanel"),
          width = 2,

          shiny::br(),
          shiny::br(),

          shiny::wellPanel(
            style = "background: #C9DFEC",

            shiny::selectInput(
              inputId   = ns("fertilizerSite"),
              label     = "fertilized site",
              choices   = desfert_sites[["code"]],
              selected  = NULL,
              multiple  = FALSE,
              selectize = FALSE
              ),
            shiny::br(),
            shiny::dateInput(
              inputId = ns("fertilizerDate"),
              label   = "date of fertilization",
              format  = "yyyy-mm-dd"
              ),
            shiny::br(),
            shiny::numericInput(
              inputId = ns("nitrogenAmount"),
              label   = "N added",
              value   = 1.715,
              min     = 0
              ),
            shiny::br(),
            shiny::numericInput(
              inputId = ns("phosphorusAmount"),
              label   = "P added",
              value   = 1.224,
              min     = 0
              ),
            shiny::br(),
            shiny::textInput(
              inputId = ns("nitrogenPhosphorusAmount"),
              label   = "N and P added",
              value   = "1.715 and 1.224"
              ),
            shiny::br(),
            shiny::actionButton(
              inputId = ns("add_fertilizer_data"),
              label   = "add fertilizer data",
              class   = "btn-success",
              style   = "color: #fff;",
              icon    = shiny::icon("plus"),
              width   = "100%"
              ),
            shiny::br()
          )  # close well panel
          ), # close left col

        shiny::column(
          id    = "column_fertilizer_data",
          width = 10,
          DT::dataTableOutput(ns("fertilizer_data_view"))
        ) # close column_fertilizer_data
      ) # close row_samples_data

    ) # close the page
  ) # close tagList

} # close viewDischargeUI


# fertilizer -------------------------------------------------------------------

fertilizer <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    # setup
    ns <- session$ns # to facilitate renderUIs
    listener_init("update_fertilizer") # does this work here instead of global?


    fertilizer_data_reactive <- shiny::reactive({

      # add listener for adding records
      listener_watch("update_fertilizer")

      fertilizer_data <- query_fertilizer()

      return(fertilizer_data)

    })


    output$fertilizer_data_view <- DT::renderDataTable({

      fertilizer_data_reactive()

    },
    class      = "cell-border stripe",
    filter     = "top",
    extensions = c("FixedHeader", "Buttons"),
    plugins    = c("ellipsis"),
    escape     = FALSE,
    selection  = "none",
    rownames   = FALSE,
    options    = list(
      autoWidth     = FALSE,
      scrollX       = FALSE,
      bLengthChange = FALSE,
      bPaginate     = TRUE,
      bSort         = TRUE,
      autoWidth     = FALSE,
      pageLength    = 50,
      fixedHeader   = FALSE,
      searching     = FALSE
    )
    ) # close table output


    # validation rules

    iv <- shinyvalidate::InputValidator$new()

    iv$add_rule("fertilizerSite", shinyvalidate::sv_required())
    iv$add_rule("fertilizerDate", shinyvalidate::sv_required())
    iv$add_rule("nitrogenAmount", shinyvalidate::sv_required())
    iv$add_rule("phosphorusAmount", shinyvalidate::sv_required())
    iv$add_rule("nitrogenPhosphorusAmount", shinyvalidate::sv_required())

    iv$add_rule("fertilizerDate", function(value) {
      if (as.Date(value) > Sys.Date()) {
        "time travel not allowed"
      }
    }
    )

    iv$add_rule("nitrogenAmount", function(value) {
      if (!is.na(value) && value < 0) {
        "must be >= 0"
      }
    }
    )

    iv$add_rule("phosphorusAmount", function(value) {
      if (!is.na(value) && value < 0) {
        "must be >= 0"
      }
    }
    )


    # add fertilizer data

    shiny::observeEvent(input$add_fertilizer_data, {

    iv$enable()
    req(iv$is_valid())

      add_fertilizer(
        fertilizer_site = input$fertilizerSite,
        fertilizer_date = as.character(input$fertilizerDate),
        fertilizer_n    = input$nitrogenAmount,
        fertilizer_p    = input$phosphorusAmount,
        fertilizer_np   = input$nitrogenPhosphorusAmount
      )


    # disable validation
    iv$disable()

    # trigger listener
    listener_trigger("update_fertilizer")

    })


    # debugging: module level -------------------------------------------------

    # observe(print({ head(fertilizer_data_reactive()) }))
    # observe(print({ queryType$default }))
    # observe(print({ input$ReachPatchs_cell_edit }))


    # close module fertilizer -------------------------------------------------

  }) # close module server
} # close module function
