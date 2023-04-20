#' @title module: add a new or edit a existing (annual) cover composition
#'
#' @description module_cover_composition_new facilitates adding a new or
#' editing an existing urbancndep.cover_composition record
#'
#' @export

module_cover_composition_new <- function(
  id,
  modal_title,
  ce_id = NULL,
  composition_to_edit
  ) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # UI -----------------------------------------------------------------------

    hold <- composition_to_edit()

    shiny::showModal(
      shiny::modalDialog(
        shiny::fluidRow(
          shiny::column(
            id     = "none",
            width  = 5,
            offset = 1,

            shiny::selectInput(
              inputId   = ns("new_cover_observation_type"),
              label     = "cover type",
              choices   = c(cover_types[["cover_type"]]),
              selected  = ifelse(is.null(hold), "", hold$cover_type),
              multiple  = FALSE,
              selectize = FALSE # ensure selectize false
              ),
            shiny::numericInput(
              inputId = ns("new_cover_observation_amount"),
              label   = "amount",
              value   = ifelse(is.null(hold), "", as.character(hold$amount)),
              min     = 0,
              max     = 1,
              step    = 0.1
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

    iv$add_rule("new_cover_observation_type",   shinyvalidate::sv_required())
    iv$add_rule("new_cover_observation_amount", shinyvalidate::sv_required())


    # add or update ------------------------------------------------------------

    shiny::observeEvent(input$submit, {

      iv$enable()
      req(iv$is_valid())

      shiny::removeModal()

      if (is.null(composition_to_edit())) {

        this_cc_id      <- NULL
        this_query_type <- "insert"

      } else {

        this_cc_id      <- composition_to_edit()$id
        this_query_type <- "update"

      }

      insert_new_or_update_cc(
        cover_type_description = input$new_cover_observation_type,
        cover_amount           = as.numeric(input$new_cover_observation_amount),
        query_type             = this_query_type,
        cover_event_id         = ce_id,
        cover_composition_id   = this_cc_id
      )

      # disable validation
      iv$disable()

      # trigger listener
      listener_trigger("update_composition")

    }) # close observeEvent::submit


    # close --------------------------------------------------------------------

  }) # close module server
} # close module function
