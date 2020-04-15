#' @title Module: fertilizer
#'
#' @description The module fertilizer facilitates...


# fertilizer UI -----------------------------------------------------------

fertilizerUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(
        HTML(paste0("#", ns("leftPanel"), "{ background: #D3D3D3; }"))
      ) # close tags$style
    ), # close tags$head
    fluidPage(
      fluidRow(
        column(
          id = ns("leftPanel"), 2,
          br(),
          br(),
          selectInput(
            inputId = ns("fertilizerSite"),
            label = "fertilized site",
            choices = c(
              "DBG",
              "MVP",
              "EME",
              "EMW",
              "LDP",
              "MCN",
              "MCS",
              "PWP",
              "SME",
              "SMW",
              "SNE",
              "SNW",
              "SRR",
              "UMP",
              "WTM"
            ),
            selected = NULL,
            multiple = FALSE
          ),
          br(),
          dateInput(
            inputId = ns("fertilizerDate"),
            label = "date of fertilization",
            format = "yyyy-mm-dd"
          ),
          br(),
          numericInput(
            inputId = ns("nitrogenAmount"),
            label = "N added",
            value = 1.715
          ),
          br(),
          numericInput(
            inputId = ns("phosphorusAmount"),
            label = "P added",
            value = 1.224
          ),
          br(),
          textInput(
            inputId = ns("nitrogenPhosphorusAmount"),
            label = "N and P added",
            value = "1.715 and 1.224"
          ),
          br(),
          actionButton(
            inputId = ns("addFertilizerData"),
            label = "add fertilizer data",
            style = "text-align:center; border-sytle:solid; border-color:#0000ff;"
          ),
          br(),
          br()
        ), # close the left col
        column(
          id = "fertilizerDataViewerRightPanel", 10,
          DT::dataTableOutput(ns("fertilizerDataOutput"))
        ) # close the right col
      ) # close the row
    ) # close the page
  ) # close tagList
  
} # close viewDischargeUI


# modify reach patches main -----------------------------------------------

fertilizer <- function(input, output, session) {
  
  fertadd <- reactiveValues(fertAdded = 0)
  
  # query existing fertdata (listener results in refresh when a sample is added)
  fertilizerData <- reactive({
    
    # add listener for adding a fert sample
    fertadd$fertAdded 
    
    baseQuery <- '
    SELECT
      s.code AS site_code,
      f.date AS application_date,
      f."N" AS nitrogen,
      f."P" AS phosphorus,
      f."N_and_P" AS nit_and_phos
    FROM urbancndep.fertilizer_applications f
    JOIN urbancndep.sites s ON (f.site_id = s.id)
    ORDER BY f.id DESC;'
    
    interpolatedQuery  <- sqlInterpolate(ANSI(),
                                         baseQuery)
    
    # establish db connection
    pg <- database_connection()
    
    fertilizerDataReturn <- dbGetQuery(pg,
                                       interpolatedQuery)
    
    # close database connection
    dbDisconnect(pg)
    
    return(fertilizerDataReturn)
    
  })
  
  # render existing queried fert data
  output$fertilizerDataOutput <- DT::renderDataTable({
    
    fertilizerData()
    
  },
  selection = 'none',
  escape = FALSE,
  server = TRUE,
  options = list(paging = TRUE,
                 pageLength = 25,
                 ordering = TRUE,
                 searching = TRUE #,
                 # order = list(list(1, 'desc'), list(0, 'asc'))
  ),
  rownames = F) # close renderDataTable
  
  
  # add fertilizer data
  observeEvent(input$addFertilizerData, {
    
    # upload requies numerous inputs
    req(
      input$fertilizerSite,
      input$fertilizerDate,
      input$nitrogenAmount,
      input$phosphorusAmount,
      input$nitrogenPhosphorusAmount
    )
    
    # text site code to numeric equivalent
    if (input$fertilizerSite == 'DBG') {
      numericSiteCode <- 2
    } else if (input$fertilizerSite == 'EME') {
      numericSiteCode <- 3
    } else if (input$fertilizerSite == 'EMW') {
      numericSiteCode <- 4
    } else if (input$fertilizerSite == 'LDP') {
      numericSiteCode <- 6
    } else if (input$fertilizerSite == 'MCN') {
      numericSiteCode <- 8
    } else if (input$fertilizerSite == 'MCS') {
      numericSiteCode <- 9
    } else if (input$fertilizerSite == 'MVP') {
      numericSiteCode <- 10
    } else if (input$fertilizerSite == 'PWP') {
      numericSiteCode <- 11
    } else if (input$fertilizerSite == 'SME') {
      numericSiteCode <- 12
    } else if (input$fertilizerSite == 'SMW') {
      numericSiteCode <- 13
    } else if (input$fertilizerSite == 'SNE') {
      numericSiteCode <- 14
    } else if (input$fertilizerSite == 'SNW') {
      numericSiteCode <- 15
    } else if (input$fertilizerSite == 'SRR') {
      numericSiteCode <- 16
    } else if (input$fertilizerSite == 'UMP') {
      numericSiteCode <- 17
    } else if (input$fertilizerSite == 'WTM') {
      numericSiteCode <- 18
    } else  {
      numericSiteCode <- NULL
    }
    
    baseFertilizerInsertQuery <- '
    INSERT INTO urbancndep.fertilizer_applications
    (
      site_id,
      date,
      "N",
      "P",
      "N_and_P"
    )
    VALUES
    (
      ?fertSite,
      ?fertDate,
      ?fertN,
      ?fertP,
      ?fertNP
    );'
    
    # build query from base and input parameters
    fertilizerInsertQuery <- sqlInterpolate(ANSI(),
                                            baseFertilizerInsertQuery,
                                            fertSite = numericSiteCode,
                                            fertDate = as.character(input$fertilizerDate),
                                            fertN = input$nitrogenAmount,
                                            fertP = input$phosphorusAmount,
                                            fertNP = input$nitrogenPhosphorusAmount)
    
    # establish db connection
    pg <- database_connection()
    
    tryCatch({
      
      dbGetQuery(pg, "BEGIN TRANSACTION")
      
      # execute insert query
      dbExecute(pg,
                fertilizerInsertQuery)
      
      
      dbCommit(pg)
      
      # change listener state when adding a record
      fertadd$fertAdded <- isolate(fertadd$fertAdded + 1)
      
    }, warning = function(warn) {
      
      showNotification(ui = paste("there is a warning:  ", warn),
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'warning')
      
      print(paste("WARNING: ", warn))
      
    }, error = function(err) {
      
      showNotification(ui = paste("there was an error:  ", err),
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error')
      
      print(paste("ERROR: ", err))
      print("ROLLING BACK TRANSACTION")
      
      dbRollback(pg)
      
    }) # close try catch
    
    # close database connection
    dbDisconnect(pg)
    
  })
  
  
  # debugging: module level -------------------------------------------------
  
  ############# START debugging
  # observe(print({ queryType }))
  # observe(print({ queryType$default }))
  # observe(print({ input$ReachPatchs_cell_edit }))
  ############# END debugging
  
  
  # close module fertilizer -------------------------------------------------
  
} # close module fertilizer 
