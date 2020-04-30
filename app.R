
# README ------------------------------------------------------------------

# see repository README


# call global R -----------------------------------------------------------

source('global.R')


# UI ----------------------------------------------------------------------

ui <- tagList(
  tags$head(
    tags$style(
      # HTML("#lachatAnnotate { border-style: solid;
      #      border-color: #0000ff; }"),
      # HTML("#mergeMetaLachat { border-style: solid;
      #      border-color: #0000ff; }"),
      # HTML("#mergeMetaLachat { border-style: solid;
      #      border-color: #0000ff; }"),
      # HTML("#lachatUpload { border-style: solid;
      #      border-color: #1aff1a; }"),
      # HTML("#uploadMetaLachat { border-style: solid;
      #      border-color: #1aff1a; }"),
      HTML("#leftPanel { background: #D3D3D3; }")
    ) # close tags$head
  ), # close tagss$style
  tags$script(
    # notable stmt
    HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
                                         Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                                        })")
  ), # close tags$script
  navbarPage("desert fertilization",
             
             # resin upload tab --------------------------------------------------------
             
             tabPanel("resin: file upload",
                      fluidPage(
                        fluidRow( 
                          column(id = 'leftPanel', 2,
                                 br(),
                                 helpText("1. identify a default date for sample collection (optional)",
                                          style = "text-align: left; color: DarkBlue; font-weight: bold"),
                                 textInput(inputId = "resinDateSeed",
                                           label = NULL,
                                           value = NULL,
                                           placeholder = "yyyy-mm-dd"),
                                 br(),
                                 helpText("2. upload Lachat file",
                                          style = "text-align: left; color: DarkBlue; font-weight: bold"),
                                 fileInput(inputId = "file1", 
                                           label = NULL,
                                           multiple = FALSE,
                                           accept = c("text/csv",
                                                      "application/vnd.ms-excel",
                                                      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
                                 helpText(id = "explainLachatUpload",
                                          "use upload lachat to upload annotated lachat data without merging"),
                                 actionButton("lachatUpload",
                                              "upload lachat"),
                                 br()
                          ), # close the left col
                          column(id = "fileUploadRightPanel", 10,
                                 # lachat annotations
                                 DT::dataTableOutput("dynamicLachat"),
                                 uiOutput("lachatPreviewDivider"),
                                 DT::dataTableOutput("dynamicLachatPreview")
                          ) # close the right col
                        ) # close the row
                      ) # close the page
             ), # close 'resin: file upload' tab panel
             
             # resin viewer tab --------------------------------------------------------
             
             tabPanel("resin: data viewer",
                      fluidPage(
                        fluidRow( 
                          column(id = 'leftPanel', 2,
                                 p("date range:",
                                   style = "text-align: left; background-color: LightGray; color: black;"),
                                 br(),
                                 dateInput("resinDataStartDate",
                                           "start:",
                                           format = "yyyy-mm-dd"),
                                 dateInput("resinDataEndDate",
                                           "end:",
                                           format = "yyyy-mm-dd"),
                                 actionButton(inputId = "filterResinObservations",
                                              label = "filter",
                                              style = "text-align:center; border-sytle:solid; border-color:#0000ff;"),
                                 actionButton(inputId = "clearFilterResinObservations",
                                              label = "clear filter",
                                              style = "text-align:center; border-sytle:solid; border-color:#0000ff;"),
                                 checkboxInput(inputId = "resinUnknownsOnly",
                                               label = "display only unknowns",
                                               value = TRUE),
                                 br()
                          ), # close the left col
                          column(id = "resinDataViewerRightPanel", 10,
                                 DT::dataTableOutput("resinDataOutput")
                          ) # close the right col
                        ) # close the row
                      ) # close the page
             ), # close 'resin: data viewer' tab panel
             
             # fertilizer tab ----------------------------------------------------------
             
             tabPanel("fertilizer",
                      fertilizerUI("fertilizerManager") 
             ) # close fertilizer module UI
             
  ) # close navbar/page
) # close tagList


server <- function(input, output, session) {
  
  # helper functions for interactive tables ---------------------------------
  
  # helper function for reading checkbox
  shinyInput <- function(FUN, len, id, ...) { 
    inputs = character(len) 
    for (i in seq_len(len)) { 
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...)) 
    } 
    inputs 
  } 
  
  # helper function for reading checkbox
  shinyValue <- function(id, len) { 
    unlist(lapply(seq_len(len), function(i) { 
      value = input[[paste0(id, i)]] 
      if (is.null(value)) NA else value 
    })) 
  } 
  
  
  # resin lachat data -------------------------------------------------------
  
  # annotate lachat ---------------------------------------------------------
  
  # use a seed date if supplied - must be supplied before loading data
  useDateSeed <- reactiveVal(FALSE)
  
  observeEvent(input$resinDateSeed, ignoreInit = FALSE, once = FALSE, {
    
    useDateSeed(TRUE)
    
  })
  
  
  # lachat data from file upload
  lachatFile <- reactive({
    
    session$sendCustomMessage('unbind-DT', 'dynamicLachat') # notable stmt
    
    req(input$file1)
    
    # require csv or Excel file type
    validate(
      need(file_ext(input$file1$datapath) %in% c("csv", "xls", "xlsx"), "data file must be of type csv, xls, or xlsx")
    )
    
    # import based on file type
    if (grepl("xls|xlsx", file_ext(input$file1$datapath))) {
      
      lachatImport <- read_excel(input$file1$datapath)
      
    } else if (grepl("csv", file_ext(input$file1$datapath))) {
      
      lachatImport <- read_csv(input$file1$datapath)
      
    }
    
    # format lachat data
    formattedLachat <- format_lachat(lachatData = lachatImport,
                                     fileName = input$file1$name)
    
    # join imported data with samples list to pre-populate sample id
    formattedLachat <- formattedLachat %>% 
      left_join(resinSamplesFrame, by = c("Sample ID" = "resinSamples")) %>% 
      rename(fieldID = databaseID)
    
    return(formattedLachat)
    
  })
  
  # add visual separator between dynamic data and preview of data to upload
  output$lachatPreviewDivider <- renderUI({
    
    req(input$file1)
    
    tagList(
      hr(),
      p("data to preview",
        style = "text-align: left; background-color: LightGray; color: black;")
    )
    
  }) 
  
  
  # render LACHAT file upload and interactive data fields
  output$dynamicLachat <- DT::renderDataTable({
    
    seedDate <- if (useDateSeed()) isolate(input$resinDateSeed) else NULL
    
    lachatFile() %>% 
      mutate(
        omit = shinyInput(checkboxInput,
                          nrow(lachatFile()),
                          "omit_",
                          value = FALSE,
                          width = "20px"),
        newFieldID  = shinyInput(selectInput,
                                 nrow(lachatFile()),
                                 "newFieldID_",
                                 choices = resinSamplesFrame$resinSamples,
                                 selected = NULL,
                                 multiple = FALSE,
                                 width = "120px"),
        collectionDate = shinyInput(textInput,
                                    nrow(lachatFile()),
                                    "collectionDate_",
                                    # value = NULL,
                                    value = seedDate,
                                    width = "120px",
                                    placeholder = "yyyy-mm-dd"),
        notes = shinyInput(textInput,
                           nrow(lachatFile()),
                           "notes_",
                           value = NULL,
                           width = "120px")
      ) %>% 
      select(notes, collectionDate, omit, newFieldID, fieldID, everything()) %>%
      filter(grepl("unknown", `Sample Type`, ignore.case = TRUE))
  },
  selection = 'none',
  escape = FALSE,
  server = FALSE,
  options = list(bFilter = 0,
                 bLengthChange = F,
                 bPaginate = F,
                 bSort = F,
                 preDrawCallback = JS('function() { 
                           Shiny.unbindAll(this.api().table().node()); }'), 
                 drawCallback = JS('function() { 
                        Shiny.bindAll(this.api().table().node()); } ') 
  ),
  rownames = F) # close renderDataTable
  
  
  # capture file upload and values provided through interactive table
  lachatWithAnnotations <- reactive({
    
    lachatFile() %>%
      mutate(
        omit = shinyValue("omit_",
                          nrow(lachatFile())),
        newFieldID = shinyValue('newFieldID_',
                                nrow(lachatFile())),
        collectionDate = shinyValue("collectionDate_",
                                    nrow(lachatFile())),
        notes = shinyValue("notes_",
                           nrow(lachatFile()))
      ) %>%
      mutate(newFieldID = as.character(newFieldID)) %>% # cast newFieldID to char to avoid case_when logical errors
      mutate(fieldID = case_when(
        grepl("unknown", `Sample Type`, ignore.case = TRUE) & grepl("blk", `Sample ID`, ignore.case = TRUE) ~ toupper(`Sample ID`),
        TRUE ~ fieldID
      )) # blanks can simply be brought over into the fieldID column
    
  })
  
  # preview lachatWithAnnotations before upload
  output$dynamicLachatPreview <- DT::renderDataTable({
    
    lachatWithAnnotations() %>%
      filter(
        grepl("unknown", `Sample Type`, ignore.case = TRUE),
        omit == FALSE
      ) %>% 
      mutate(
        newFieldID = replace(newFieldID, newFieldID == "NULL", NA),
        fieldID = case_when(
          !is.na(newFieldID) ~ newFieldID,
          TRUE ~ fieldID
        )
      ) %>% 
      select(notes, collectionDate, fieldID, `Sample ID`, `Sample Type`, `Cup Number`, `Analyte Name`, `Peak Concentration`)
    
  },
  selection = 'none',
  escape = FALSE,
  server = FALSE,
  options = list(bFilter = 0,
                 bLengthChange = F,
                 bPaginate = F,
                 bSort = F
  ),
  rownames = F) # close output$dynamicLachatPreview
  
  
  # upload annotated --------------------------------------------------------
  
  observeEvent(input$lachatUpload, {
    
    # set tryCatch to fail if there are invalid date types; other checks are
    # embedded within the tryCatch
    tryCatch({
      
      lachatToUpload <- lachatWithAnnotations() %>%
        mutate(
          newFieldID = replace(newFieldID, newFieldID == "NULL", NA),
          fieldID = case_when(
            !is.na(newFieldID) ~ newFieldID,
            TRUE ~ fieldID
          ),
          collectionDate = replace(collectionDate, collectionDate == "", NA),
          collectionDate = case_when(
            grepl("blk", fieldID, ignore.case = T) ~ as.Date(NA),
            TRUE ~ as.Date(collectionDate)
          ),
          notes = replace(notes, notes == "", NA),
          omit = as.logical(omit)
        )
      
      # run a series of data validations (in addition to the tryCatch for
      # confirming valid dates)
      
      # 1. check if any unknowns not flagged to omit are missing a fieldID or
      # date; notify user if so else upload to database
      if (
        any(
          lachatToUpload$`Sample Type` %in% c('Unknown', 'unknown') &
          lachatToUpload$omit == FALSE &
          !grepl("BLK", lachatToUpload$`Sample ID`, ignore.case = T) &
          (is.na(lachatToUpload$fieldID) | is.na(lachatToUpload$collectionDate))
        )
      ) {
        
        showNotification(ui = "at least one unknown missing fieldID, collection date, or flag to omit",
                         duration = NULL,
                         closeButton = TRUE,
                         type = 'error')
        
        # 2. check for duplicates, combination of combination of fieldID,
        # collectionDate, Analyte Name, omit must be unique
      } else if (
        
        anyDuplicated(
          lachatToUpload[
            grepl("unknown", lachatToUpload$`Sample Type`, ignore.case = TRUE) &
            !grepl("BLK", lachatToUpload$`Sample ID`, ignore.case = T), 
            c("fieldID", "collectionDate", "Analyte Name", "omit")
            ]
        )
        
      ) {
        
        showNotification(ui = "at least one duplicate: fieldID x collectionDate x Analyte Name x omit",
                         duration = NULL,
                         closeButton = TRUE,
                         type = 'error')
        
        # 4. call data_upload, which also has a tryCatch, if all checks passed
      } else {
        
        data_upload(lachatToUpload)
        
      } # close data validation and call to upload
      
    }, error = function(err) {
      
      showNotification(ui = "at least one collection date is not in expected date format",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error')
      
    }) # close tryCatch
    
  })
  
  
  # resin samples viewer ----------------------------------------------------
  
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
      
      baseResinDataQuery <- '
      SELECT *
      FROM
        urbancndep.resin
      ORDER BY upload_batch DESC, id ASC;
      '
      
      resinDataQuery  <- sqlInterpolate(ANSI(),
                                        baseResinDataQuery)
      
    } else {
      
      baseResinDataQuery <- '
      SELECT *
      FROM
        urbancndep.resin
      WHERE 
        collection_date BETWEEN ?start AND ?end
      ORDER BY upload_batch DESC, id ASC;
      '
      
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
                       type = 'warning')
      
    } else {
      
      resinToDisplay <- resinData()
      
    }
    
    if (input$resinUnknownsOnly == TRUE) {
      
      resinToDisplay <- resinToDisplay %>% 
        filter(grepl("Unknown", sample_type, ignore.case = TRUE))
      
    } 
    
    return(resinToDisplay)    
    
  },
  selection = 'none',
  escape = FALSE,
  server = TRUE,
  options = list(paging = TRUE,
                 pageLength = 25,
                 ordering = TRUE,
                 searching = TRUE),
  rownames = F) # close output$resinDataOutput
  
  
  # fertilizer --------------------------------------------------------------
  
  callModule(module = fertilizer,
             id = "fertilizerManager")  
  
  
  # debugging ---------------------------------------------------------------
  
  # observe(print({ colnames(lachatFile()) }))
  # observe(print({ lachatFile() }))
  # observe(print({ lachatWithAnnotations() %>% select(contains(c("field", "id"))) %>% print(n=Inf) }))
  # observe(print({ str(lachatWithAnnotations()) }))
  # observe(print({ lachatWithAnnotations() %>% select(contains("field")) }))
  # observe(print({ metadataFile() }))
  # observe(print({ mergedData() }))
  # observe(print({ mergedWithAnnotations() }))
  # observe(print({ str(mergedWithAnnotations()) }))
  # observe(print({ colnames(mergedWithAnnotations()) }))
  
  
} # close server

# Run the application 
shinyApp(ui = ui, server = server)