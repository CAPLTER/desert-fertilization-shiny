#' @title module: add annual biomass record
#'
#' @description module_annual_biomass_new facilitates adding a new or editing
#' an existing urbancndep.annuals_biomass record.
#'
#' @export

module_annual_biomass_new <- function(
  id,
  modal_title,
  ab_to_edit
  ) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # UI -----------------------------------------------------------------------

    hold <- ab_to_edit()

    shiny::showModal(
      shiny::modalDialog(
        shiny::fluidRow(
          shiny::column(
            id     = "col_add_survey_left",
            width  = 5,
            offset = 1,

            # ensure all selectize false!
            shiny::selectInput(
              inputId   = ns("new_biomass_plot"),
              label     = "plot",
              choices   = cover_sites_plots["plot_id"],
              selected  = ifelse(is.null(hold), "", hold$plot_id),
              multiple  = FALSE,
              selectize = FALSE
              ),
            shiny::selectInput(
              inputId   = ns("new_biomass_lwp"),
              label     = "location within plot",
              choices   = c("P", "IP"),
              selected  = ifelse(is.null(hold), "", hold$location_within_plot),
              multiple  = FALSE,
              selectize = FALSE
              ),
            shiny::selectInput(
              inputId   = ns("new_biomass_replicate"),
              label     = "replicate",
              choices   = c(1, 2),
              selected  = ifelse(is.null(hold), "", hold$replicate),
              multiple  = FALSE,
              selectize = FALSE
              ),
            shiny::selectInput(
              inputId   = ns("new_biomass_orientation"),
              label     = "subquad orientation",
              choices   = c("N", "S", "W", "E"),
              selected  = ifelse(is.null(hold), "", hold$subquad_orientation),
              multiple  = FALSE,
              selectize = FALSE
              ),
            shiny::dateInput(
              inputId = ns("new_biomass_date"),
              label   = "collection date",
              value   = ifelse(is.null(hold), as.character(Sys.Date()), as.character(hold$date)),
              format  = "yyyy-mm-dd",
              min     = "2022-01-01",
              max     = as.character(Sys.Date())
              ),
            shiny::numericInput(
              inputId = ns("new_biomass_mass"),
              label   = "mass",
              value   = ifelse(is.null(hold), "", as.character(hold$mass)),
              min     = 0,
              max     = 1,
              step    = 0.1
              ),
            shiny::textAreaInput(
              inputId = ns("new_biomass_notes"),
              label   = "notes",
              value   = ifelse(
                test = is.na(hold$notes),
                yes  = "",
                no   = as.character(hold$notes)
                ),
              placeholder = "free-text notes",
              resize      = "vertical"
              ),
            shiny::selectInput(
              inputId   = ns("new_biomass_quadrat"),
              label     = "quadrat",
              choices   = c(1:4),
              selected  = ifelse(is.null(hold), "", hold$quadrat),
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

    iv$add_rule("new_biomass_plot",         shinyvalidate::sv_required())
    iv$add_rule("new_biomass_lwp",          shinyvalidate::sv_required())
    iv$add_rule("new_biomass_replicate",    shinyvalidate::sv_required())
    iv$add_rule("new_biomass_orientation",  shinyvalidate::sv_required())
    iv$add_rule("new_biomass_date",         shinyvalidate::sv_required())
    iv$add_rule("new_biomass_mass",         shinyvalidate::sv_required())
    iv$add_rule("new_biomass_quadrat",      shinyvalidate::sv_required())

    iv$add_rule("new_biomass_date", ~ if (!lubridate::is.Date(.)) "not a valid date")
    iv$add_rule("new_biomass_date", function(value) {
      if (as.Date(value) > Sys.Date()) { "time travel not allowed" }
    }
    )


    # add or update ------------------------------------------------------------

    shiny::observeEvent(input$submit, {

      iv$enable()
      req(iv$is_valid())

      shiny::removeModal()

      if (is.null(ab_to_edit())) {

        this_ab_id      <- NULL
        this_query_type <- "insert"

      } else {

        this_ab_id      <- ab_to_edit()$id
        this_query_type <- "update"

      }

      insert_new_or_update_ab(
        annual_biomass_plot        = as.integer(input$new_biomass_plot),
        annual_biomass_lwp         = as.character(input$new_biomass_lwp),
        annual_biomass_replicate   = as.integer(input$new_biomass_replicate),
        annual_biomass_orientation = as.character(input$new_biomass_orientation),
        annual_biomass_date        = as.character(input$new_biomass_date),
        annual_biomass_mass        = as.numeric(input$new_biomass_mass),
        annual_biomass_notes       = as.character(input$new_biomass_notes),
        annual_biomass_quadrat     = as.integer(input$new_biomass_quadrat),
        query_type                 = this_query_type,
        ab_id                      = this_ab_id
      )

      if (this_query_type == "insert") {

        shiny::updateTextAreaInput(
          inputId     = "new_biomass_notes",
          label       = "notes",
          value       = "",
          placeholder = "free-text notes",
        )

      }

      # disable validation
      iv$disable()

      # trigger listener
      listener_trigger("update_annuals_biomass")

    }) # close observeEvent::submit


    # close --------------------------------------------------------------------

  }) # close module server
} # close module function
