#' @title module: cover events
#'
#' @description The module cover events facilitates entering Desert
#'  Fertilization annuals cover data. This interface is written explicity as a
#'  data-entry tool and purposefully omits more sophisiticated data querying and
#'  viewing functionality.

# UI ---------------------------------------------------------------------------

cover_events_UI <- function(id) {

  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(
        HTML(
          paste0("#", ns("left_panel"), "{ background: #D3D3D3; color: #484848; }")
        ) # close HTML
      ) # close tags$style
      ), # close tagss$head
    fluidPage(
      fluidRow(
        column(
          id = ns("left_panel"), 2,

          # add new

          strong("add new",
            style = "text-align: center; color: black"),
          selectizeInput(
            inputId = ns("new_cover_event_plot"),
            label = "plot (required)",
            choices = cover_sites_plots["plot_id"],
            selected = NULL,
            multiple = FALSE
            ),
          selectizeInput(
            inputId = ns("new_cover_event_position"),
            label = "position (required)",
            choices = c("P", "IP"),
            selected = NULL,
            multiple = FALSE
            ),
          selectizeInput(
            inputId = ns("new_cover_event_subplot"),
            label = "subplot (required)",
            choices = c(1, 2),
            selected = NULL,
            multiple = FALSE
            ),
          dateInput(
            inputId = ns("new_cover_event_date"),
            "collection date (required)",
            format = "yyyy-mm-dd"
            ),
          selectizeInput(
            inputId = ns("new_cover_event_collector"),
            label = "collector (required)",
            choices = c("QS", "MG"),
            selected = c("QS", "MG"),
            multiple = TRUE
            ),
          br(),
          actionButton(
            inputId = ns("add_new_cover_event"),
            label = "add event",
            style = "text-align:center; border-sytle:solid; border-color:#0000ff;"
          )
          ), # close the left column

        # main view

        column(
          id = "right_panel",
          width = 10,
          DT::dataTableOutput(ns("cover_events_view")),
          div(id = "add_cover_compositions")
        ) # close the right column
      ) # close the row
    ) # close the page
  ) # close tagList

} # close UI


# main -------------------------------------------------------------------------

cover_events_server <- function(input, output, session) {

  # added to facilitate renderUIs
  ns <- session$ns

  # create listener for adding and deleting records
  listen_cover_events <- reactiveValues(db_version = 0)


  # cover events data ------------------------------------------------------------

  cover_events_reactive <- reactive({

    # add listener for adding and deleting records
    listen_cover_events$db_version

    base_query <- "
    SELECT
      cover_events.cover_event_id AS id,
      sites.code AS site,
      cover_events.plot AS plot,
      treatments.code AS treatment,
      cover_events.patch_type AS position,
      cover_events.subplot,
      cover_events.sample_date AS date,
      cover_events.year,
      cover_events.collector
    FROM
      urbancndep.cover_events
    JOIN urbancndep.plots ON (cover_events.plot = plots.id)
    JOIN urbancndep.sites ON (sites.id = plots.site_id)
    JOIN urbancndep.treatments treatments ON (treatments.id = plots.treatment_id)
    WHERE
      cover_events.year >= ?year
    ORDER BY
      cover_events.cover_event_id DESC ;
    "

    parameterized_query <- sqlInterpolate(
      ANSI(),
      base_query,
      year = lubridate::year(Sys.Date())
    )

    cover_events_data <- run_interpolated_query(parameterized_query)

    cover_events_data <- cover_events_data %>%
      mutate(
        modify = shinyInput(
          reactiveObject = cover_events_data,
          FUN = actionButton,
          len = nrow(cover_events_data),
          id = "",
          label = "observations",
          onclick = sprintf('Shiny.setInputValue("%s",  this.id)', session$ns("button_modify_cover_event"))),
        delete = shinyInput(
          reactiveObject = cover_events_data,
          FUN = actionButton,
          len = nrow(cover_events_data),
          id = "",
          label = "delete",
          onclick = sprintf('Shiny.setInputValue("%s",  this.id)', session$ns("button_delete_cover_event")))
      )

    return(cover_events_data)

  }) # close cover_events_reactive


  # render (non-editable) table of cover events data
  output$cover_events_view <- DT::renderDT({

    cover_events_reactive()

  },
  escape = FALSE,
  selection = "none",
  rownames = FALSE,
  options = list(
    columnDefs = list(list(targets = c(0, 9, 10), orderable = FALSE)),
    bFilter = 0,
    bLengthChange = FALSE,
    bPaginate = FALSE,
    autoWidth = TRUE
  )
  ) # close output$cover_events_view


  # delete cover events ----------------------------------------------------------

  delete_cover_event <- function(row_to_delete) {

    baseQuery <- "
    DELETE FROM urbancndep.cover_events
    WHERE cover_event_id = ?RWE_ID ;
    "

    parameterized_query <- sqlInterpolate(
      ANSI(),
      baseQuery,
      RWE_ID = as.numeric(row_to_delete)
    )

    run_interpolated_execution(parameterized_query)

    # change listener state when deleting a record
    listen_cover_events$db_version <- isolate(listen_cover_events$db_version + 1)

  }

  observeEvent(input$button_delete_cover_event, {

    delete_cover_event(row_to_delete = input$button_delete_cover_event)

    })


  # add new cover event ----------------------------------------------------------

  observeEvent(input$add_new_cover_event, {

    validate(
      need(!is.null(input$new_cover_event_plot), "select a sample position"),
      need(!is.null(input$new_cover_event_position), "select a sample position"),
      need(!is.null(input$new_cover_event_subplot), "select a sample subplot"),
      need(!is.null(input$new_cover_event_date), "enter a collection date"),
      need(is.Date(input$new_cover_event_date), "collection date must be a valid date"),
      need(!is.null(input$new_cover_event_collector), "enter a collector"),
      need(paste0(c(input$new_cover_event_collector), collapse = "; ") != "", "enter a collector")
    )

    input_new_date <- as.character(input$new_cover_event_date)
    input_new_year <- as.integer(lubridate::year(input$new_cover_event_date))
    input_new_plot <- as.integer(input$new_cover_event_plot)
    input_new_patch_type <- as.character(input$new_cover_event_position)
    input_new_subplot <- as.integer(input$new_cover_event_subplot)
    input_new_collector <- paste0(c(input$new_cover_event_collector), collapse = "; ")

    # new cover event
    base_query <- "
    INSERT INTO urbancndep.cover_events
    (
      sample_date,
      year,
      plot,
      patch_type,
      subplot,
      collector
    )
    VALUES
    (
      ?submitted_date,
      ?submitted_year,
      ?submitted_plot,
      ?submitted_patch_type,
      ?submitted_subplot,
      ?submitted_collector
      ) ;
    "

    parameterized_query <- sqlInterpolate(
      ANSI(),
      base_query,
      submitted_date = input_new_date,
      submitted_year = input_new_year,
      submitted_plot = input_new_plot,
      submitted_patch_type = input_new_patch_type,
      submitted_subplot = input_new_subplot,
      submitted_collector = input_new_collector
    )

    # establish db connection
    pg <- database_connection()

    # because we want multiple processes wrapped in a tryCatch, we cannot use
    # run_interpolated_execution()

    tryCatch({

      # add record keeper to composition table

      dbGetQuery(pg, "BEGIN TRANSACTION")

      # execute insert query
      dbExecute(
        pg,
        parameterized_query
      )

      # get id of added event
      get_event_id_base_query <- "
      SELECT
      MAX (cover_event_id)
      FROM urbancndep.cover_events
      ;
      "
      get_event_id_parameterized_query <- sqlInterpolate(
        ANSI(),
        get_event_id_base_query
      )

      new_event_id <- dbGetQuery(pg, get_event_id_parameterized_query)
      new_event_id <- as.integer(new_event_id)

      # add composition record keeping observation
      add_record_base_query <- "
      INSERT INTO urbancndep.cover_composition
      (
        cover_event_id,
        cover_type_id,
        cover_amt
      )
      VALUES
      (
        ?submitted_event_id,
        111,
        1
        ) ;
      "
      add_record_parameterized_query <- sqlInterpolate(
        ANSI(),
        add_record_base_query,
        submitted_event_id = new_event_id
      )

      dbExecute(pg, add_record_parameterized_query)

      # commit transaction
      dbCommit(pg)

      showNotification(
        ui = "action committed",
        duration = 3,
        closeButton = TRUE,
        type = "message")

      # change listener state when deleting a record
      listen_cover_events$db_version <- isolate(listen_cover_events$db_version + 1)

      # reset values of input form

      updateSelectizeInput(
        session,
        inputId = "new_cover_event_plot",
        label = "plot (required)",
        choices = cover_sites_plots["plot_id"],
        selected = NULL
      )
      updateSelectizeInput(
        session,
        inputId = "new_cover_event_position",
        label = "position (required)",
        choices = c("P", "IP"),
        selected = NULL
      )
      updateSelectizeInput(
        session,
        inputId = "new_cover_event_subplot",
        label = "subplot (required)",
        choices = c(1, 2),
        selected = NULL
      )
      updateDateInput(
        session,
        inputId = "new_cover_event_date",
        label = "collection date (required)",
        value = Sys.Date()
      )
      updateSelectizeInput(
        session,
        inputId = "new_cover_event_collector",
        label = "collector (required)",
        choices = annuals_comp_surveyors,
        selected = annuals_comp_surveyors
      )

    }, warning = function(warn) {

      showNotification(
        ui = paste("there is a warning:  ", warn),
        duration = NULL,
        closeButton = TRUE,
        type = "warning")

      print(paste("WARNING: ", warn))

    }, error = function(err) {

      showNotification(
        ui = paste("there was an error:  ", err),
        duration = NULL,
        closeButton = TRUE,
        type = "error")

      print(paste("ERROR: ", err))
      print("ROLLING BACK TRANSACTION")

      # rollback upon error
      dbRollback(pg)

    }) # close try catch

    # close database connection
    dbDisconnect(pg)

    })

  # add observations to cover event ----------------------------------------------

  # establish counter for removing module UIs
  cover_composition_counter <- reactiveVal(value = 0)

  # action on modify cover event

  observeEvent(input$button_modify_cover_event, {

    # module counter
    cover_composition_module_id <- cover_composition_counter()

    # unique element id based on module counter
    cover_element_id <- paste0("cover_element_id_", cover_composition_module_id)

    # insert composition module at placeholder with a unique ID;
    # wrap UIs in a div so we can easily call the div tag id to selectively
    # remove the module UIs
    insertUI(
      selector = "#add_cover_compositions",
      where = "beforeBegin",
      ui = tags$div(
        id = cover_element_id,
        cover_composition_UI(ns(paste0("cover_event", cover_composition_module_id)))
      )
    )

    # call cover composition module with unique id
    callModule(
      module = cover_composition,
      id = paste0("cover_event", cover_composition_module_id),
      cover_event_ID = reactive({ input$button_modify_cover_event })
    )

    # increment module counter
    cover_composition_counter(cover_composition_module_id + 1)

    # remove composition module if one already exist
    if (cover_composition_counter() > 1) { removeUI(selector = paste0("#", cover_element_id)) }

    })


  # debugging: module level -------------------------------------------------

  ############# START debugging
  # observe(print({ cover_events_reactive() }))
  # observe(print({ queryType$default }))
  # observe(print({ input$new_cover_event_collector }))
  ############# END debugging

} # close module::cover_events
