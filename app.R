
# README ------------------------------------------------------------------

# Desert Fertilization application to address (1) current, backlog, and future
# uploads of resin data, and (2) fertilizer applications.

# The approach taken here for the resin data is to upload the raw lachat data
# and marry sample metadata directly to the machine output. This is different
# than, for example, the stormwater data where raw lachat data are loaded then
# results only are pulled out and coupled to a sampling event. The approach here
# is advantageous in that the techs can handle all of the data entry. On the
# pull side, we can query only unknowns not flagged for omit, then parse the
# sample ID, which is standardized based on drop-down input. Of course, the
# approach works better here (than, for example, stormwater) as there is a
# one-to-one relationship between a sample and result, whereas there is a
# one-to-many relationship between samples and result (Lachat, ICP, AQ2, etc.)
# that is handled much better with a sampling event.

# Resin Lachat data can be married to metadata using one of two approaches: (1)
# manually enter all metadata details (site, date, notes), or (2) merge metadata
# entered into an Excel file, and manually enter only information not available
# from the merge. As a site and date are required for all Lachat unknowns are
# required for upload, the latter is a far more efficient approach, at least in
# those cases where metadata has already been entered.

# Fertilizer is a simple upload and viewer.

# issues:
# 1. cannot get dateInput type for shinyInput value

# call global R -----------------------------------------------------------

source('global.R')


# UI ----------------------------------------------------------------------

ui <- tagList(
  tags$head(
    tags$style(
      # in use
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
                                 fileInput("file1", "choose lachat file",
                                           multiple = FALSE,
                                           accept = c("text/csv",
                                                      "application/vnd.ms-excel",
                                                      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
                                 # actionButton("lachatAnnotate",
                                 #              "ANNOTATE lachat"),
                                 helpText(id = "explainLachatUpload",
                                          "use upload lachat to upload annotated lachat data without merging"),
                                 actionButton("lachatUpload",
                                              "upload lachat"),
                                 br(),
                                 hr(),
                                 fileInput("file2", "choose metadata file",
                                           multiple = FALSE,
                                           accept = c("text/csv",
                                                      "application/vnd.ms-excel",
                                                      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
                                 textInput(inputId = "metadataWorksheet", 
                                           label = "metadata worksheet",
                                           value = "",
                                           placeholder = "e.g., Winter2015"),
                                 actionButton("importMetadata",
                                              "1. import metadata"),
                                 br(),
                                 br(),
                                 actionButton("mergeMetaLachat",
                                              "2. merge metadata+lachat"),
                                 br(),
                                 br(),
                                 actionButton("uploadMetaLachat",
                                              "3. upload metadata+lachat"),
                                 br(),
                                 br()
                          ), # close the left col
                          column(id = "fileUploadRightPanel", 10,
                                 # lachat annotations
                                 DT::dataTableOutput("dynamicLachat"),
                                 uiOutput("lachatPreviewDivider"),
                                 DT::dataTableOutput("dynamicLachatPreview"),
                                 # lachat merged
                                 DT::dataTableOutput("dynamicMerged"),
                                 uiOutput("mergedPreviewDivider"),
                                 DT::dataTableOutput("dynamicMergedPreview")
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
                                 br(),
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
  
  # LACHAT data from file upload
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
    
    lachatFile() %>% 
      mutate(
        omit = shinyInput(checkboxInput,
                          nrow(lachatFile()),
                          "omit_",
                          value = FALSE,
                          width = "20px"),
        fieldID = shinyInput(selectInput,
                             nrow(lachatFile()),
                             "fieldID_",
                             choices = resinSamples,
                             selected = NULL,
                             multiple = FALSE,
                             width = "120px"),
        collectionDate = shinyInput(textInput,
                                    nrow(lachatFile()),
                                    "collectionDate_",
                                    value = NULL,
                                    width = "120px",
                                    placeholder = "yyyy-mm-dd"),
        notes = shinyInput(textInput,
                           nrow(lachatFile()),
                           "notes_",
                           value = NULL,
                           width = "120px")
      ) %>% 
      select(notes, collectionDate, omit, fieldID, `Sample ID`, `Sample Type`, `Cup Number`, `Analyte Name`, `Peak Concentration`) %>%
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
        fieldID = shinyValue('fieldID_',
                             nrow(lachatFile())),
        collectionDate = shinyValue("collectionDate_",
                                    nrow(lachatFile())),
        notes = shinyValue("notes_",
                           nrow(lachatFile()))
      )
    
  })
  
  # preview lachatWithAnnotations before upload
  output$dynamicLachatPreview <- DT::renderDataTable({
    
    lachatWithAnnotations() %>%
      filter(
        grepl("unknown", `Sample Type`, ignore.case = TRUE),
        omit == FALSE
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
    
    # modify data object as needed for the DB
    lachatToUpload <- lachatWithAnnotations() %>%
      mutate(
        fieldID = replace(fieldID, fieldID == "NULL", NA),
        collectionDate = replace(collectionDate, collectionDate == "", NA),
        collectionDate = as.Date(collectionDate),
        notes = replace(notes, notes == "", NA),
        omit = as.logical(omit)
      )
    
    # check if any unknowns not flagged to omit are missing a fieldID or Date;
    # notify user if so else upload to database
    if (
      any(
        lachatToUpload$`Sample Type` %in% c('Unknown', 'unknown') &
        lachatToUpload$omit == FALSE &
        (is.na(lachatToUpload$fieldID) | is.na(lachatToUpload$collectionDate))
      )
    ) { 
      
      showNotification(ui = "at least one unknown missing fieldID/collection date, or flag to omit",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error')
      
      # check for duplicates, combination of combination of fieldID,
      # collectionDate, Analyte Name, omit must be unique
    } else if (
      anyDuplicated(lachatToUpload[grepl("unknown", lachatToUpload$`Sample Type`, ignore.case = TRUE), c("fieldID", "collectionDate", "Analyte Name", "omit")])
    ) {
      
      showNotification(ui = "at least one duplicate: fieldID x collectionDate x Analyte Name x omit",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error')
      
    } else {
      
      # data_upload(lachatToUpload)
      
      showNotification(ui = "currently disabled",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error')
      
    } # close missing fieldID or data if-else
    
  })
  
  
  # metadata file -----------------------------------------------------------
  
  # metadata from file upload
  metadataFile <- eventReactive(input$importMetadata, {
    
    req(
      input$file2,
      input$metadataWorksheet
    )
    
    validate(
      need(file_ext(input$file2$datapath) %in% c("xls", "xlsx"), "data file must be of type xls, or xlsx")
    )
    
    tryCatch({
      
      metadataFromFile <- import_metadata(
        input$file2$datapath,
        input$metadataWorksheet
      )
      
      showNotification(ui = "successfully imported",
                       duration = 5,
                       closeButton = TRUE,
                       type = 'message')
      
    }, warning = function(warn) {
      
      showNotification(ui = paste("imported with warning:  ", warn),
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'warning')
      
      print(paste("WARNING:", warn))
      
    }, error = function(err) {
      
      showNotification(ui = paste("error: ", err),
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error',
                       action = a(href = "javascript:location.reload();", "reload the page"))
      
      print(paste("ERROR:", err))
      
    }) # close try catch
    
    return(metadataFromFile)
    
  }) # close eventReactive metadataFile 
  
  
  # resin merge -------------------------------------------------------------
  
  # attempt merge of uploaded meta- and lachat data
  mergedData <- eventReactive(input$mergeMetaLachat, {
    
    # merging requires successful imports and processing of field and lab data
    req(
      lachatFile(),
      metadataFile()
    )
    
    # check that the number of rows following the join would not create a data
    # entity with more rows than the original lachat file
    if (nrow(lachatFile()) != nrow(right_join(metadataFile(), lachatFile(), by = c("fieldID" = "Sample ID")))) { 
      
      mergedLabAndField <- NULL
      
      showNotification(ui = "these data could not be merged",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error',
                       action = a(href = "javascript:location.reload();", "reload the page"))
      
    } else {
      
      # remove previous UIs if merge can proceed
      removeUI(selector = "#dynamicLachat")
      removeUI(selector = "#lachatPreviewDivider")
      removeUI(selector = "#dynamicLachatPreview")
      
      # merge lab and field data based on fieldID and idToJoin (copy of lachat
      # Sample ID); NULL collection dates indicate a match was not found -
      # nullify fieldID based on this criterion
      mergedLabAndField <- right_join(metadataFile(),
                                      lachatFile(),
                                      by = c("fieldID" = "idToJoin")) %>% 
        mutate(fieldID = replace(fieldID, is.na(collectionDate), NA))
      
      showNotification(ui = "okay, wait for merge results",
                       duration = 5,
                       closeButton = TRUE,
                       type = "warning")
      
    }
    
    return(mergedLabAndField)
    
  })
  
  
  # add visual separator between dynamic data and preview of data to upload
  output$mergedPreviewDivider <- renderUI({
    
    req(input$mergeMetaLachat)
    
    tagList(
      hr(),
      p("data to preview",
        style = "text-align: left; background-color: LightGray; color: black;")
    )
    
  }) 
  
  # render merged data and interactive data fields
  output$dynamicMerged <- DT::renderDataTable({
    
    # ensure merged data not = null
    validate(
      need(!is.null(mergedData()), message = "merged data is empty")
    )
    
    mergedData() %>% 
      mutate(
        omit = shinyInput(checkboxInput,
                          nrow(lachatFile()),
                          "omitthis_",
                          value = FALSE,
                          width = "20px"),
        newFieldID = shinyInput(selectInput,
                                nrow(lachatFile()),
                                "newFieldID_",
                                choices = resinSamples,
                                selected = NULL,
                                multiple = FALSE,
                                width = "120px"),
        newDate = shinyInput(textInput,
                             nrow(lachatFile()),
                             "newDate_",
                             value = NULL,
                             width = "120px",
                             placeholder = "yyyy-mm-dd"),
        newNotes = shinyInput(textInput,
                              nrow(lachatFile()),
                              "newNotes_",
                              value = NULL,
                              width = "120px")
      ) %>% 
      select(newNotes,
             notes,
             newDate,
             collectionDate,
             omit,
             newFieldID,
             fieldID,
             `Sample ID`,
             `Sample Type`,
             `Cup Number`,
             `Analyte Name`,
             `Peak Concentration`) %>%
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
  
  
  # capture merged data and values provided through interactive table
  mergedWithAnnotations <- reactive({
    
    mergedData() %>%
      mutate(
        omit = shinyValue("omitthis_",
                          nrow(mergedData())),
        newFieldID = shinyValue('newFieldID_',
                                nrow(mergedData())),
        newDate = shinyValue('newDate_',
                             nrow(mergedData())),
        newNotes = shinyValue('newNotes_',
                              nrow(mergedData()))
      ) %>% 
      # merge any old and new notes
      mutate(
        newNotes = as.character(newNotes)
      ) %>%
      mutate(notes = case_when(
        is.na(notes) & !is.na(newNotes) ~ newNotes,
        !is.na(notes) & is.na(newNotes) ~ notes,
        notes != "" & newNotes != "" ~ paste0(notes, "; ", newNotes),
        TRUE ~ notes )
      )
    
  })
  
  
  # preview mergedWithAnnotations before upload
  output$dynamicMergedPreview <- DT::renderDataTable({
    
    mergedWithAnnotations() %>%
      filter(
        grepl("unknown", `Sample Type`, ignore.case = TRUE),
        omit == FALSE
      ) %>% 
      select(notes, collectionDate, newDate, fieldID, `Sample ID`, newFieldID, `Sample Type`, `Analyte Name`, `Peak Concentration`)
    
  },
  selection = 'none',
  escape = FALSE,
  server = FALSE,
  options = list(bFilter = 0,
                 bLengthChange = F,
                 bPaginate = F,
                 bSort = F
  ),
  rownames = F) # close output$dynamicMergedPreview
  
  
  # upload merged -----------------------------------------------------------
  
  observeEvent(input$uploadMetaLachat, {
    
    # modify data object as needed for the DB
    mergedToUpload <- mergedWithAnnotations() %>%
      mutate(
        newFieldID = replace(newFieldID, newFieldID == "NULL", NA),
        newDate = replace(newDate, newDate == "", NA),
        notes = replace(notes, notes == "", NA),
        fieldID = case_when(
          !is.na(newFieldID) ~ newFieldID,
          TRUE ~ fieldID
        ),
        collectionDate = case_when(
          !is.na(newDate) ~ newDate,
          TRUE ~ collectionDate
        ),
        collectionDate = as.Date(collectionDate),
        omit = as.logical(omit)
      )
    
    # check if any unknowns not flagged to omit are missing a fieldID or Date;
    # notify user if so else upload to database
    if (
      any(
        mergedToUpload$`Sample Type` %in% c('Unknown', 'unknown') &
        mergedToUpload$omit == FALSE &
        !grepl("BLK", mergedToUpload$`Sample ID`, ignore.case = T) &
        (is.na(mergedToUpload$fieldID) | is.na(mergedToUpload$collectionDate))
      )
    ) { 
      
      showNotification(ui = "at least one unknown missing fieldID/collection date, or flag to omit",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error')
      
      # check for duplicates, combination of combination of fieldID,
      # collectionDate, and Analyte Name must be unique.
      # This check updated 2018-08-27 to remove omit from the comparison.
    } else if (
      
      anyDuplicated(
        mergedToUpload %>% 
        filter(
          grepl("unknown", `Sample Type`, ignore.case = TRUE),
          !grepl("blk", fieldID, ignore.case = TRUE),
          is.na(omit)
        ) %>% 
        select(fieldID, collectionDate, `Analyte Name`)
      )
      
    ) {
      
      showNotification(ui = "at least one duplicate: fieldID x collectionDate x Analyte Name x omit",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error')
      
    } else {
      
      data_upload(mergedToUpload)
      
    } # close missing fieldID or data if-else
    
  }) # close uploadMetaLachat
  
  
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
      
      resinToDisplay <- tibble(col = NA)
      
      showNotification(ui = "there are not any data in that date range",
                       duration = 5,
                       closeButton = TRUE,
                       type = 'warning')
      
    } else {
      
     resinToDisplay <- resinData()
      
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
  
  # observe(print({ shinyValue("omit_",
  #                            nrow(lachatFile())) }))
  # observe(print({ shinyValue("notes_",
  #                            nrow(lachatFile())) }))
  # observe(print({ colnames(lachatWithAnnotations()) }))
  # observe(print({ lachatWithAnnotations() %>%
  #     filter(grepl("unknown", `Sample Type`, ignore.case = TRUE)) %>%
  #     select(samples, collDate, notes, `Sample ID`:sourceFile) }))
  # observe(print({ metadataFile() }))
  # observe(print({ str(mergedWithAnnotations()) }))
  # observe(print({ colnames(mergedWithAnnotations()) }))
  # observe(print({ mergedWithAnnotations() %>%
  #     filter(grepl("unknown", `Sample Type`, ignore.case = TRUE)) %>%
  #     select(newDate, collectionDate) }))
  # observe(print({ input$queryAllResin }))
  # observe(print({ addedFert() }))
  # observe(print({ addFert() }))
  
  
} # close server

# Run the application 
shinyApp(ui = ui, server = server)