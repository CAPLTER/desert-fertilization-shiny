#' @title module: (annual) cover event new
#'
#' @description module_cover_event_new facilitates adding a new or editing an
#' existing urbancndep.cover_events record
#'
#' @export

module_cover_event_new <- function(
  id,
  modal_title,
  ce_to_edit
  ) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns
    message("from ce new: ", session$ns(id))

    # UI -----------------------------------------------------------------------

    hold <- ce_to_edit()

    shiny::showModal(
      shiny::modalDialog(
        shiny::fluidRow(
          shiny::column(
            id     = "col_add_survey_left",
            width  = 5,
            offset = 1,

            # ensure all selectize false!
            shiny::selectInput(
              inputId   = ns("new_cover_event_plot"),
              label     = "plot",
              choices   = cover_sites_plots["plot_id"],
              selected  = ifelse(is.null(hold), "", hold$plot),
              multiple  = FALSE,
              selectize = FALSE
              ),
            shiny::selectInput(
              inputId   = ns("new_cover_event_position"),
              label     = "position",
              choices   = c("P", "IP"),
              selected  = ifelse(is.null(hold), "", hold$position),
              multiple  = FALSE,
              selectize = FALSE
              ),
            shiny::selectInput(
              inputId   = ns("new_cover_event_subplot"),
              label     = "subplot",
              choices   = c(1, 2),
              selected  = ifelse(is.null(hold), "", hold$subplot),
              multiple  = FALSE,
              selectize = FALSE
              ),
            shiny::dateInput(
              inputId = ns("new_cover_event_date"),
              label   = "collection date",
              value   = ifelse(is.null(hold), as.character(Sys.Date()), as.character(hold$date)),
              format  = "yyyy-mm-dd",
              min     = "2022-01-01",
              max     = as.character(Sys.Date())
              ),
            shiny::selectInput(
              inputId   = ns("new_cover_event_collector"),
              label     = "collector(s)",
              choices   = annuals_comp_surveyors,
              selected  = ifelse(is.null(hold), "", hold$collector),
              multiple  = TRUE,
              selectize = FALSE
            )

          )  # close column
          ), # close fluidRow
        title  = modal_title,
        size   = "l",
        footer = list(
          shiny::actionButton(
            inputId = ns("submit"),
            label   = "submit",
            class   = "btn btn-primary",
            style   = "color: white"
            ),
          shiny::modalButton("cancel")
          ), # close footer
        easyClose = TRUE
      ) # close modalDialog
    )   # close showModal


    # validation ---------------------------------------------------------------

    iv <- shinyvalidate::InputValidator$new()

    iv$add_rule("new_cover_event_plot",       shinyvalidate::sv_required())
    iv$add_rule("new_cover_event_position",   shinyvalidate::sv_required())
    iv$add_rule("new_cover_event_subplot",    shinyvalidate::sv_required())
    iv$add_rule("new_cover_event_collector",  shinyvalidate::sv_required())
    iv$add_rule("new_cover_event_date",       shinyvalidate::sv_required())

    iv$add_rule("new_cover_event_date", ~ if (!lubridate::is.Date(.)) "not a valid date")
    iv$add_rule("new_cover_event_date", function(value) {
      if (as.Date(value) > Sys.Date()) { "time travel not allowed" }
    }
    )


    # add or update ------------------------------------------------------------

    shiny::observeEvent(input$submit, {

      iv$enable()
      req(iv$is_valid())

      shiny::removeModal()

      if (is.null(ce_to_edit())) {

        this_ce_id   <- NULL
        this_ce_type <- "insert"

      } else {

        this_ce_id   <- ce_to_edit()$id
        this_ce_type <- "update"

      }

      insert_new_or_update_ce(
        cover_event_plot      = as.integer(input$new_cover_event_plot),
        cover_event_position  = as.character(input$new_cover_event_position),
        cover_event_subplot   = as.integer(input$new_cover_event_subplot),
        cover_event_collector = paste0(c(input$new_cover_event_collector), collapse = "; "),
        cover_event_date      = as.character(input$new_cover_event_date),
        query_type            = this_ce_type,
        ce_id                 = this_ce_id
      )

      # disable validation
      iv$disable()

      # trigger listener
      listener_trigger("update_cover_events")

    }) # close observeEvent::submit


    # close --------------------------------------------------------------------

  }) # close module server
} # close module function
