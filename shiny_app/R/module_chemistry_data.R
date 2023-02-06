#' @title module: view desert fertilization related chemistry data
#'
#' @description The module \code{chemistry_data} facilitates viewing chemistry
#' data related to the desert fertilization project for review.
#'
#' @note Data corresponding to the resin collectors, and CHN plant tissue are
#' queryable.
#'
#' @export
#'

# chemistry_dataUI -------------------------------------------------------------

chemistry_dataUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidPage(

      shiny::fluidRow(

        shiny::column(
          width = 12
          ),
        shiny::column(
          width = 2,
          shiny::selectInput(
            inputId   = ns("data_source"),
            label     = "which data would you like to view?",
            choices   = c("resin", "chn"),
            selected  = NULL,
            multiple  = FALSE,
            selectize = FALSE
            ),
        )  # close column
        ), # close fluidRow

      shiny::fluidRow(

        shiny::column(
          width = 12,
          DT::dataTableOutput(ns("chemistry_data_view"))
        )

      ) # close fluidRow
    )   # close fluidPage

  ) # close taglist

}


# chemistry_data ---------------------------------------------------------------

chemistry_data <- function(id, tab = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

    # chemistry_data_reactive <- shiny::reactive({
    chemistry_data_reactive <- shiny::eventReactive(input$data_source, {

      if (input$data_source == "resin") {

        chemistry_data <- query_resin()

      } else if (input$data_source == "chn") {

        chemistry_data <- query_chn()

      } else {

        chemistry_data <- NULL

      }


      return(chemistry_data)

    })


    output$chemistry_data_view <- DT::renderDataTable({

      chemistry_data_reactive()

    },
    class      = c("compact" ,"cell-border stripe"),
    filter     = "top",
    extensions = c("FixedHeader"),
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
      fixedHeader   = TRUE,
      searching     = FALSE #,
      # columnDefs    = list(
      #   list(
      #     targets = c(2, 16),
      #     render  = JS("$.fn.dataTable.render.ellipsis( 25 )")
      #     ),
      #   list(
      #     targets = c(5, 6),
      #     width   = "50px"
      #   )
      # )
    )
    ) # close table output


    # debugging: module level ------------------------------------------------

    # observe(readr::write_csv({ rawReactive() }, "/tmp/lachat_raw.csv"))
    # observe(readr::write_csv({ resultReactive() }, "/tmp/lachat_results_reactive.csv"))
    # observe(print({ head(chemistry_data_reactive()) }))
    # observe(print({ colnames(raw_reactive()) }))
    # observe(print({ dplyr::glimpse(raw_reactive()) }))


    # close module -----------------------------------------------------------

  }) # close module server
}    # close module function
