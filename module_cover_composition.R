#' @title module: cover composition
#'
#' @description The module cover composition facilitates adding Desert
#'  Fertilization cover composition data to cover events.

# cover composition UI ---------------------------------------------------------

cover_composition_UI <- function(id) {

  ns <- NS(id)

  tagList(
    uiOutput(ns("cover_composition_styling")),
    DT::DTOutput(ns("cover_composition_view")),
    uiOutput(ns("add_new_cover_composition_UI"))
  ) # close tagList

} # close cover_composition_UI

# cover composition ------------------------------------------------------------

cover_composition <- function(input, output, session, cover_event_ID) {

  # added to facilitate renderUIs
  ns <- session$ns

  # create listener for adding and deleting records
  listen_cover_composition <- reactiveValues(db_version = 0)

  cover_composition_data <- reactive({

    # add listener for adding and deleting records
    listen_cover_composition$db_version

    baseQuery <- "
    SELECT
      cover_composition.cover_id AS id,
      cover_events.cover_event_id,
      cover_types.cover_type,
      cover_types.cover_category,
      cover_composition.cover_amt as amount
    FROM urbancndep.cover_composition
    JOIN urbancndep.cover_events ON (cover_composition.cover_event_id = cover_events.cover_event_id)
    JOIN urbancndep.cover_types ON (cover_composition.cover_type_id = cover_types.cover_type_id)
    WHERE
      cover_composition.cover_event_id = ?cover_event
    ORDER BY
      cover_events.cover_event_id,
      cover_types.cover_category DESC,
      cover_types.cover_type
    ;
    "
    parameterized_query <- sqlInterpolate(
      ANSI(),
      baseQuery,
      cover_event = cover_event_ID()
    )

    cover_composition_observations <- run_interpolated_query(parameterized_query)

    # add delete button to cover composition observation
    cover_composition_observations <- cover_composition_observations %>%
      mutate(
        delete = shinyInput(
          reactiveObject = cover_composition_observations,
          FUN = actionButton,
          len = nrow(cover_composition_observations),
          id = "",
          label = "delete",
          onclick = sprintf('Shiny.setInputValue("%s",  this.id)', session$ns("button_delete_cover_composition_observation"))
        )
      )

    return(cover_composition_observations)

  })


  # render (uneditable) table of cover composition observations
  output$cover_composition_view <- DT::renderDT({

    cover_composition_data()

  },
  escape = FALSE,
  selection = "none",
  rownames = FALSE,
  options = list(
    columnDefs = list(list(targets = c(1, 5), orderable = FALSE)),
    bFilter = 0,
    bLengthChange = FALSE,
    bPaginate = FALSE
  )

  ) # close cover_composition_view


# delete cover composition observation -----------------------------------------

  delete_cover_composition_observation <- function(row_to_delete) {

    base_query <- "
    DELETE FROM urbancndep.cover_composition
    WHERE cover_composition.cover_id = ?RWE_ID ;
    "

    parameterized_query <- sqlInterpolate(
      ANSI(),
      base_query,
      RWE_ID = as.numeric(row_to_delete)
    )

    run_interpolated_execution(parameterized_query)

    # change listener state when deleting a record
    listen_cover_composition$db_version <- isolate(listen_cover_composition$db_version + 1)

  }


  observeEvent(input$button_delete_cover_composition_observation, {

    delete_cover_composition_observation(row_to_delete = input$button_delete_cover_composition_observation)

  })


# add new cover composition observation ----------------------------------------

  output$cover_event_under_edit <- renderText({ cover_event_ID() })

  # generate UI for adding a new cover composition observation
  output$add_new_cover_composition_UI <- renderUI({

    tagList(
      tags$head(
        tags$style(
          HTML(paste0("#", ns("cover_event_under_edit"), "{ color: DarkGray; }"))
        ) # close tags$style
        ), # close tagss$head
      fluidRow(
        id = "new_cover_observation_row",
        column(
          width = 2,
          tags$b("cover_event_ID"),
          p(""),
          textOutput(ns("cover_event_under_edit"))
          ),
        column(
          width = 4,
          selectizeInput(
            ns("new_cover_observation_type"),
            "cover type",
            choices = c(cover_types$cover_type),
            selected = FALSE,
            multiple = FALSE
          )
          ),
        column(
          width = 2,
          numericInput(
            ns("new_cover_observation_amount"),
            label = "amount",
            value = NULL,
            min = 0,
            max = 1
            )
          ),
          column(
            width = 2,
            style = "margin-top: 25px",
            actionButton(
              ns("add_new_cover_observation"),
              label = "add new"
            )
          ) # close last column
        ) # close fluidRow
      ) # close tag list

  })


  observeEvent(input$add_new_cover_observation, {

    validate(
      need(!is.null(input$new_cover_observation_type), "add a type"),
      need(!is.null(input$new_cover_observation_amount), "add an amount"),
      need(input$new_cover_observation_amount != 0, "amount cannot be zero"),
      need(
        input$new_cover_observation_amount <= 1,
        message = "amount must be <= 1"
      )
    )

    base_query <- "
    INSERT INTO urbancndep.cover_composition
    (
      cover_event_id,
      cover_type_id,
      cover_amt
    )
    VALUES
    (
      ?submitted_cover_event_id,
      ?submitted_cover_type_id,
      ?submitted_cover_amt
    ) ;
    "

    this_cover_event_id <- as.integer(cover_event_ID())
    this_cover_type_id <- glue::glue_sql("{cover_types[cover_types$cover_type %in% c(input$new_cover_observation_type), ]$cover_type_id*}", .con = DBI::ANSI())
    this_observation_amount <- as.numeric(input$new_cover_observation_amount)

    parameterized_query <- sqlInterpolate(
      ANSI(),
      base_query,
      submitted_cover_event_id = this_cover_event_id,
      submitted_cover_type_id = this_cover_type_id,
      submitted_cover_amt = this_observation_amount
    )

    run_interpolated_execution(parameterized_query)

    # change listener state when deleting a record
    listen_cover_composition$db_version <- isolate(listen_cover_composition$db_version + 1)

    # reset values of input form (not working)

    updateSelectizeInput(
      session,
      inputId = "new_cover_observation_type",
      label = "cover type",
      choices = c(cover_types$cover_type),
      selected = NULL
    )
    updateNumericInput(
      session,
      inputId = "new_cover_observation_amount",
      label = "amount",
      value = NULL
    )

    }) # close observe add_new_cover_observation


# new observation UI dressing --------------------------------------------------

  # render module details if module is called

  output$cover_composition_styling <- renderUI({

    tagList(
      hr(),
      p("cover observations",
        style = "text-align: left; background-color: LightGray; color: black;")
    )

  })

} # close module cover_composition
