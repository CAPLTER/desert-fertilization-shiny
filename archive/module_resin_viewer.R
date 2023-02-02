#' @title Module: ResinViewer
#'
#' @description ResinViewer is an R6 class that facilitates a module for
#' viewing resin sample data.
#'
#' @export
#'

ResinViewer <- R6Class("ResinViewer", list(

  # attributes
  id = NULL,

  # initalizer
  initialize = function(id) {

    self$id <- id

  },

  # UI
  ui = function() {

    ns <- shiny::NS(self$id)

    shiny::fluidPage(
      shiny::fluidRow(

        shiny::column(
          id = "resin_data_view_column", 12,
          DT::dataTableOutput(ns("resin_data_view"))
        )

      ) # close the row
    ) # close the page

  }, # close ui

  # server
  server = function(input, output, session) {

    resin_data_reactive <- shiny::reactive({

      resin_data <- query_resin()

      return(resin_data)

    })


    # render discharge data for viewing
    output$resin_data_view <- DT::renderDataTable({

      resin_data_reactive()

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
      pageLength    = 100,
      fixedHeader   = FALSE,
      searching     = FALSE,
      columnDefs    = list(
        list(
          targets = c(2, 16),
          render  = JS("$.fn.dataTable.render.ellipsis( 25 )")
          ),
        list(
          targets = c(5, 6),
          width   = "50px"
        )
      )
    )
    ) # close table output

    },

  # call
  call = function(input, ouput, session) {

    callModule(self$server, self$id)

  }

) # close public
) # close R6::ResinViewer

ResinViewer1 <- ResinViewer$new(id = "resin_display")
