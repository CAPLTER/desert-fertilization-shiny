
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


# shiny options -----------------------------------------------------------

options(shiny.maxRequestSize = 30)


# libraries ---------------------------------------------------------------

library(shiny)
library(readxl)
library(DT)
library(tools)
library(tidyverse)
library(DBI)
library(RPostgreSQL)


# helper functions and config settings ------------------------------------
source('resin_samples.R')
source('import_metadata.R') 
source('format_lachat.R')
source('config.R')
source('data_upload.R')


# UI ----------------------------------------------------------------------

ui <- tagList(
  tags$head(
    tags$style(
      # in use
      HTML("#leftPanel { background: #D3D3D3; }"),
      HTML("#lachatAnnotate { border-style: solid;
           border-color: #0000ff; }"),
      HTML("#mergeMetaLachat { border-style: solid;
           border-color: #0000ff; }"),
      HTML("#mergeMetaLachat { border-style: solid;
           border-color: #0000ff; }"),
      HTML("#lachatUpload { border-style: solid;
           border-color: #1aff1a; }"),
      HTML("#uploadMetaLachat { border-style: solid;
           border-color: #1aff1a; }")
    ) # close tags$head
  ), # close tagss$style
  navbarPage("desert fertilization",
             
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
                                 actionButton("lachatAnnotate",
                                              "ANNOTATE lachat"),
                                 br(),
                                 br(),
                                 actionButton("lachatUpload",
                                              "UPLOAD lachat"),
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
                                              "IMPORT metadata"),
                                 br(),
                                 br(),
                                 actionButton("mergeMetaLachat",
                                              "MERGE meta & lachat"),
                                 br(),
                                 br(),
                                 actionButton("uploadMetaLachat",
                                              "UPLOAD meta & lachat"),
                                 br(),
                                 br()
                          ), # close the left col
                          column(id = "fileUploadRightPanel", 10,
                                 DT::dataTableOutput("lachatPreview"),
                                 br(),
                                 br(),
                                 DT::dataTableOutput("metadataPreview"),
                                 DT::dataTableOutput("dynamicLachat"),
                                 tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
          Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                            })")), # notable stmt
                                 hr(),
                                 DT::dataTableOutput("dynamicMerged")
                          ) # close the right col
                        ) # close the row
                      ) # close the page
             ), # close 'resin: file upload' tab panel
             
             tabPanel("resin: data viewer",
                      fluidPage(
                        fluidRow( 
                          column(id = 'leftPanel', 2,
                                 tags$b("date range:",
                                        style = "text-align:center"),
                                 br(),
                                 br(),
                                 dateInput("resinDataStartDate",
                                           "start:",
                                           format = "yyyy-mm-dd"),
                                 dateInput("resinDataEndDate",
                                           "end:",
                                           format = "yyyy-mm-dd"),
                                 checkboxInput(inputId = "queryAllResin",
                                               label = "view all (ignores date range)",
                                               value = FALSE),
                                 actionButton(inputId = "queryResinData",
                                              label = "view",
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
             
             tabPanel("fertilizer",
                      fluidPage(
                        fluidRow( 
                          column(id = 'leftPanel', 2,
                                 br(),
                                 br(),
                                 selectInput(inputId = "fertilizerSite",
                                             label = "fertilized site",
                                             choices = c('DBG',
                                                         'EME',
                                                         'EMW',
                                                         'LDP',
                                                         'MCN',
                                                         'MCS',
                                                         'PWP',
                                                         'SME',
                                                         'SMW',
                                                         'SNE',
                                                         'SNW',
                                                         'SRR',
                                                         'UMP',
                                                         'WTM'),
                                             selected = NULL,
                                             multiple = FALSE),
                                 br(),
                                 dateInput(inputId = "fertilizerDate",
                                           label = "date of fertilization",
                                           format = "yyyy-mm-dd"),
                                 br(),
                                 numericInput(inputId = "nitrogenAmount",
                                              label = "N added",
                                              value = 1.715),
                                 br(),
                                 numericInput(inputId = "phosphorusAmount",
                                              label = "P added",
                                              value = 1.224),
                                 br(),
                                 textInput(inputId = "nitrogenPhosphorusAmount",
                                           label = "N and P added",
                                           value = "1.715 and 1.224"),
                                 br(),
                                 actionButton(inputId = "addFertilizerData",
                                              label = "add fertilizer data",
                                              style = "text-align:center; border-sytle:solid; border-color:#0000ff;"),
                                 br(),
                                 br()
                          ), # close the left col
                          column(id = "fertilizerDataViewerRightPanel", 10,
                                 DT::dataTableOutput("fertilizerDataOutput")
                          ) # close the right col
                        ) # close the row
                      ) # close the page
             ) # close 'fertilizer' tab panel
             
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
  
  
  # preview of imported lachat data
  output$lachatPreview <- DT::renderDataTable({
    
    head(lachatFile())
    
  },
  selection = 'none',
  escape = FALSE,
  server = TRUE,
  options = list(paging = TRUE,
                 pageLength = 25,
                 ordering = FALSE,
                 searching = FALSE),
  rownames = F) # close renderDataTable
  
  
  # render LACHAT file upload and interactive data fields
  output$dynamicLachat <- DT::renderDataTable({
    
    # load lachat file into memory but only render on click
    req(input$lachatAnnotate)
    
    # if annotating, remove lachat preview
    removeUI(selector = "#lachatPreview")
    
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
      mutate(omit = shinyValue("omit_",
                               nrow(lachatFile())),
             fieldID = shinyValue('fieldID_',
                                  nrow(lachatFile())),
             collectionDate = shinyValue("collectionDate_",
                                         nrow(lachatFile())),
             notes = shinyValue("notes_",
                                nrow(lachatFile()))
      )
    
  })
  
  
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
      any(lachatToUpload$`Sample Type` %in% c('Unknown', 'unknown') &
          lachatToUpload$omit == FALSE &
          (is.na(lachatToUpload$fieldID) | is.na(lachatToUpload$collectionDate)))
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
      
      data_upload(lachatToUpload)
      
    } # close missing fieldID or data if-else
    
  })
  
  # DEBUGGING ---------------------------------------------------------------
  
  # observe(print({ shinyValue("omit_",
  #                            nrow(lachatFile())) }))
  # observe(print({ shinyValue("notes_",
  #                            nrow(lachatFile())) }))
  # observe(print({ colnames(lachatWithAnnotations()) }))
  # observe(print({ lachatWithAnnotations() %>%
  #     filter(grepl("unknown", `Sample Type`, ignore.case = TRUE)) %>%
  #     select(samples, collDate, notes, `Sample ID`:sourceFile) }))
  # observe(print({ metadataFile() }))
  # observe(print({ colnames(mergedWithAnnotations()) }))
  # observe(print({ mergedWithAnnotations() %>% 
  #     filter(grepl("unknown", `Sample Type`, ignore.case = TRUE)) %>% 
  #     select(newDate, collectionDate) }))
  # observe(print({ input$queryAllResin }))
  # observe(print({ addedFert() }))
  # observe(print({ addFert() }))
  
  ####
  
  
  # resin metadata ----------------------------------------------------------
  
  # metadata from file upload
  metadataFile <- eventReactive(input$importMetadata, {
    
    req(input$file2,
        input$metadataWorksheet)
    
    validate(
      need(file_ext(input$file2$datapath) %in% c("xls", "xlsx"), "data file must be of type xls, or xlsx")
    )
    
    tryCatch({
      
      import_metadata(input$file2$datapath,
                      input$metadataWorksheet)
      
    }, error = function(err) {
      
      showNotification(ui = paste("error: ", err),
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error',
                       action = a(href = "javascript:location.reload();", "reload the page"))
      
    }) # close try catch
    
  }) # close eventReactive
  
  
  # preview imported metadata
  output$metadataPreview <- DT::renderDataTable({
    
    head(metadataFile())
    
  },
  selection = 'none',
  escape = FALSE,
  server = TRUE,
  options = list(paging = TRUE,
                 pageLength = 25,
                 ordering = FALSE,
                 searching = FALSE),
  rownames = F) # close renderDataTable
  
  
  # resin merge -------------------------------------------------------------
  
  # attempt merge of uploaded meta- and lachat data
  mergedData <- eventReactive(input$mergeMetaLachat, {
    
    # merging requires successful imports and processing of field and lab data
    req(lachatFile(),
        metadataFile())
    
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
      removeUI(selector = "#lachatPreview")
      removeUI(selector = "#metadataPreview")
      
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
  
  # render merged data and interactive data fields
  output$dynamicMerged <- DT::renderDataTable({
    
    # require existence of a merged data
    # need to make this not just merged data but merged data not = null
    req(mergedData())
    
    mergedData() %>% 
      mutate(
        omit = shinyInput(checkboxInput,
                          nrow(lachatFile()),
                          "omit_",
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
      mutate(omit = shinyValue("omit_",
                               nrow(mergedData())),
             newFieldID = shinyValue('newFieldID_',
                                     nrow(mergedData())),
             newDate = shinyValue('newDate_',
                                  nrow(mergedData())),
             newNotes = shinyValue('newNotes_',
                                   nrow(mergedData()))
      )
    
  })
  
  
  observeEvent(input$uploadMetaLachat, {
    
    # modify data object as needed for the DB
    mergedToUpload <- mergedWithAnnotations() %>%
      mutate(
        newFieldID = replace(newFieldID, newFieldID == "NULL", NA),
        newDate = replace(newDate, newDate == "", NA),
        newNotes = replace(newNotes, newNotes == "", NA),
        fieldID = case_when(
          !is.na(newFieldID) ~ newFieldID,
          TRUE ~ fieldID
        ),
        collectionDate = case_when(
          !is.na(newDate) ~ newDate,
          TRUE ~ collectionDate
        ),
        notes = case_when(
          !is.na(newNotes) ~ newNotes,
          TRUE ~ notes 
        ),
        collectionDate = as.Date(collectionDate),
        omit = as.logical(omit)
      )
    
    # check if any unknowns not flagged to omit are missing a fieldID or Date;
    # notify user if so else upload to database
    if (
      any(mergedToUpload$`Sample Type` %in% c('Unknown', 'unknown') &
          mergedToUpload$omit == FALSE &
          (is.na(mergedToUpload$fieldID) | is.na(mergedToUpload$collectionDate)))
    ) { 
      
      showNotification(ui = "at least one unknown missing fieldID/collection date, or flag to omit",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error')
      
      # check for duplicates, combination of combination of fieldID,
      # collectionDate, Analyte Name, omit must be unique
    } else if (
      anyDuplicated(mergedToUpload[grepl("unknown", mergedToUpload$`Sample Type`, ignore.case = TRUE), c("fieldID", "collectionDate", "Analyte Name", "omit")])
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
  resinData <- eventReactive(input$queryResinData, {
    
    # establish db connection
    pg <- database_connection()
    
    if (input$queryAllResin == TRUE) {
      
      baseResinDataQuery <- '
      SELECT *
      FROM
        urbancndep.resin;
      '
      
      resinDataQuery  <- sqlInterpolate(ANSI(),
                                        baseResinDataQuery)
      
    } else {
      
      baseResinDataQuery <- '
      SELECT *
      FROM
        urbancndep.resin
      WHERE 
        collection_date BETWEEN ?start AND ?end;
      '
      
      resinDataQuery  <- sqlInterpolate(ANSI(),
                                        baseResinDataQuery,
                                        start = as.character(input$resinDataStartDate),
                                        end = as.character(input$resinDataEndDate))
      
    }
    
    isolate(
      resinDataReturn <- dbGetQuery(pg,
                                    resinDataQuery)
    )
    
    
    dbDisconnect(pg)
    
    # rather than a simple return, we need to address conditions when there are
    # not any data that match the search criteria
    
    # format the date if there are data
    if (nrow(resinDataReturn) >= 1) {
      
      resinDataReturn <- resinDataReturn 
      
      # else build an empty frame with just NAs
    } else {
      
      resinDataReturn <- data.frame(
        sample_id = "no samples match those criteria"
      )
      
    }
    
    return(resinDataReturn)
    
  }) 
  
  
  # render discharge data for viewing
  output$resinDataOutput <- DT::renderDataTable({
    
    resinData()
    
  },
  selection = 'none',
  escape = FALSE,
  server = TRUE,
  options = list(paging = TRUE,
                 pageLength = 25,
                 ordering = TRUE,
                 searching = TRUE),
  rownames = F) # close renderDataTable
  
  
  # fertilizer --------------------------------------------------------------
  
  # create listener for adding fert samples
  fertadd <- reactiveValues(fertAdded = 0)
  
  # query existing fertilizer data (includes listener to refresh when a new
  # sample is added)
  fertilizerData <- reactive({
    
    # add listener for adding a fert sample
    fertadd$fertAdded 
    
    # establish db connection
    pg <- database_connection()
    
    baseFertilizerDataQuery <- '
      SELECT
        s.code as site_code,
        f.date as application_date,
        f."N" as nitrogen,
        f."P" as phosphorus
      FROM urbancndep.fertilizer_applications f
      JOIN urbancndep.sites s ON (f.site_id = s.id)
      ;'
    
    fertilizerDataQuery  <- sqlInterpolate(ANSI(),
                                           baseFertilizerDataQuery)
    
    isolate(
      fertilizerDataReturn <- dbGetQuery(pg,
                                         fertilizerDataQuery)
    )
    
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
                 searching = TRUE,
                 order = list(list(1, 'desc'), list(0, 'asc'))
  ),
  rownames = F) # close renderDataTable
  
  
  # add fertilizer data
  observeEvent(input$addFertilizerData, {
    
    # upload requies numerous inputs
    req(input$fertilizerSite,
        input$fertilizerDate,
        input$nitrogenAmount,
        input$phosphorusAmount,
        input$nitrogenPhosphorusAmount)
    
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
    
    # establish db connection
    pg <- database_connection()
    
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
    
    tryCatch({
      
      # begin transaction
      dbGetQuery(pg, "BEGIN TRANSACTION")
      
      # execute query
      isolate(dbExecute(pg, fertilizerInsertQuery))
      
      dbCommit(pg)
      
      showNotification(ui = "successfully uploaded",
                       duration = 8,
                       closeButton = TRUE,
                       type = 'message')
      
      # change listener state when adding a fert sample
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
  
  
} # close server

# Run the application 
shinyApp(ui = ui, server = server)
