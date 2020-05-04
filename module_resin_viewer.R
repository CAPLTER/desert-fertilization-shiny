#' @title Module: ResinViewer 
#' 
#' @description ResinViewer is an R6 class that facilitates a module for viewing
#'   resin sample data. An instance of this class is generated in global then
#'   accessed from app.

ResinViewer <- R6Class("ResinViewer", list(
  
  # attributes
  id = NULL,
  
  # initalizer
  initialize = function(id) {
    
    self$id <- id
    
  },
  
  # UI
  ui = function() {
    
    ns <- NS(self$id)
    
    fluidPage(
      fluidRow(
        column(id = "leftPanel", 2,
               p("date range:",
                 style = "text-align: left; background-color: LightGray; color: black;"),
               br(),
               dateInput(ns("resinDataStartDate"),
                         "start:",
                         format = "yyyy-mm-dd"),
               dateInput(ns("resinDataEndDate"),
                         "end:",
                         format = "yyyy-mm-dd"),
               actionButton(inputId = ns("filterResinObservations"),
                            label = "filter",
                            style = "text-align:center; border-sytle:solid; border-color:#0000ff;"),
               actionButton(inputId = ns("clearFilterResinObservations"),
                            label = "clear filter",
                            style = "text-align:center; border-sytle:solid; border-color:#0000ff;"),
               checkboxInput(inputId = ns("resinUnknownsOnly"),
                             label = "display only unknowns",
                             value = TRUE),
               br()
        ), # close the left col
        column(id = "resinDataViewerRightPanel", 10,
               DT::dataTableOutput(ns("resinDataOutput"))
        ) # close the right col
      ) # close the row
    ) # close the page
    
  }, # close ui
  
  # server
  server = function(input, output, session) {
    
    # query discharge data from database for viewing
    
    # queryType: default vs parameterized query for observations
    resinQueryType <- reactiveValues(default = "default")
    
    # actionButton filterObservations = parameterized query type
    observeEvent(input$filterResinObservations, {
      
      resinQueryType$default <- "param"
      
    })
    
    # actionButton clearFilterObservations = default query type
    observeEvent(input$clearFilterResinObservations, {
      
      resinQueryType$default <- "default"
      
    })
    
    resinData <- reactive({
      
      if (resinQueryType$default == "default") {
        
        baseResinDataQuery <- "
        SELECT *
        FROM
          urbancndep.resin
        ORDER BY
          upload_batch DESC,
          id ASC;"
        
        resinDataQuery  <- sqlInterpolate(ANSI(),
                                          baseResinDataQuery)
        
      } else {
        
        baseResinDataQuery <- "
        SELECT *
        FROM
          urbancndep.resin
        WHERE
          collection_date BETWEEN ?start AND ?end
        ORDER BY
          upload_batch DESC,
          id ASC;"
        
        resinDataQuery <- sqlInterpolate(
          ANSI(),
          baseResinDataQuery,
          start = as.character(isolate(input$resinDataStartDate)),
          end = as.character(isolate(input$resinDataEndDate))
        )
        
      }
      
      # establish db connection
      pg <- database_connection()
      
      # run query
      resinDataReturn <- dbGetQuery(pg,
                                    resinDataQuery)
      
      # disconnect from db
      dbDisconnect(pg)
      
      # rather than a simple return, we need to address conditions when there are
      # not any data that match the search criteria
      
      return(resinDataReturn)
      
    })
    
    
    # render discharge data for viewing
    output$resinDataOutput <- DT::renderDataTable({
      
      # return empty frame and message if empty set
      if (nrow(resinData()) == 0) {
        
        resinToDisplay <- tibble(sample_type = NA)
        
        showNotification(ui = "there are not any data in that date range",
                         duration = 5,
                         closeButton = TRUE,
                         type = "warning")
        
      } else {
        
        resinToDisplay <- resinData()
        
      }
      
      if (input$resinUnknownsOnly == TRUE) {
        
        resinToDisplay <- resinToDisplay %>%
          filter(grepl("Unknown", sample_type, ignore.case = TRUE))
        
      }
      
      return(resinToDisplay)
      
    },
    selection = "none",
    escape = FALSE,
    server = TRUE,
    options = list(paging = TRUE,
                   pageLength = 25,
                   ordering = TRUE,
                   searching = TRUE),
    rownames = F) # close output$resinDataOutput
    
  },
  
  # call
  call = function(input, ouput, session) {
    
    callModule(self$server, self$id)
    
  }
  
) # close public

)  # close R6::ResinViewer