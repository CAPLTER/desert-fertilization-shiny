#' @title module: upload Lachat resin data
#'
#' @description The module upload_lachat facilitates uploading lachat data. The
#' user attaches the appropriate sample details to uploaded data. Upon
#' execution, the munged data with sample and analysis details are written to
#' urbancndep.resin.
#'
#' @export

# upload UI ---------------------------------------------------------------

upload_resinUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidPage(
      shiny::fluidRow(

        shiny::column(
          id = "leftPanel",
          width = 2,

          shiny::column(
            id = "leftPanel", 2,
            machineInputUI(ns("samples_for_resin")) # ns(wrap call to inner mod)
            ), # close the left col

          # shiny::br(),
          # shiny::helpText(
          #   "1. identify a default date for sample collection (optional)",
          #   style = "text-align: left; color: DarkBlue; font-weight: bold"
          #   ),
          # shiny::textInput(
          #   inputId = "resinDateSeed",
          #   label = NULL,
          #   value = NULL,
          #   placeholder = "yyyy-mm-dd"
          #   ),
          # shiny::br(),
          # shiny::helpText(
          #   "2. upload Lachat file",
          #   style = "text-align: left; color: DarkBlue; font-weight: bold"
          #   ),
          # shiny::fileInput(
          #   inputId = "lachat_file",
          #   label = NULL,
          #   multiple = FALSE,
          #   accept = c(
          #     "text/csv",
          #     "application/vnd.ms-excel",
          #     "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
          #     )
          #   ),
          # shiny::helpText(
          #   id = "explainLachatUpload",
          #   "use upload lachat to upload annotated lachat data without merging"
          #   ),
          # shiny::actionButton(
          #   "lachatUpload",
          #   "upload lachat"
          #   ),
          # shiny::br()
          # ), # close the left col

        shiny::column(
          id = "fileUploadRightPanel",
          width = 10,
          # lachat annotations
          DT::dataTableOutput("results_reactive"),
          uiOutput("lachatPreviewDivider"),
          DT::dataTableOutput("results_metadata_preview")
        ) # close the right col

      ) # close the row
    ) # close the page
  ) # close taglist

} # close upload_resinUI


# upload main -------------------------------------------------------------

upload_resin <- function(id, tab = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

    # call module machineInput: builds sample list & machine file import
    machineInputs <- machineInput("samples_for_resin")

    # helper function for reading input functions; for reasons that are not
    # clear, this function only works if included in the .R file from which it
    # is called.
    shinyValue <- function(id, len) {
      unlist(lapply(seq_len(len), function(i) {
          value = input[[paste0(id, i)]]
          if (is.null(value)) NA else value
        }))
    }


    # use a seed date if supplied - must be supplied before loading data
    # useDateSeed <- reactiveVal(FALSE)

    # observeEvent(input$resinDateSeed, ignoreInit = FALSE, once = FALSE, {

    #   useDateSeed(TRUE)

    # })


  # lachat data from file upload

    raw_reactive <- shiny::reactive({

      shiny::req(input$lachat_file)

      lachat_file_type <- tools::file_ext(input$lachat_file$datapath)

      if (!lachat_file_type %in% c("csv", "xls", "xlsx")) {

        shiny::showNotification(
          ui          = "file must be of type Excel or csv",
          duration    = NULL,
          closeButton = TRUE,
          type        = "warning",
          action      = a(href = "javascript:location.reload();", "reload the page")
        )

      } else {

        if (grepl("xls|xlsx", lachat_file_type)) {

          lachat_import <- readxl::read_excel(input$lachat_file$datapath)

        } else if (grepl("csv", lachat_file_type)) {

          lachat_import <- readr::read_csv(input$lachat_file$datapath)

        }

      }

      # format lachat data
      lachat_formatted <- format_lachat(
        lachatData = lachat_import,
        fileName   = input$lachat_file$name
      )

      # join imported data with samples list to pre-populate sample id
      lachat_formatted <- lachat_formatted |>
      dplyr::left_join(
        resinSamplesFrame,
        by = c("Sample ID" = "resinSamples")
        ) |>
      dplyr::rename(fieldID = databaseID)

      return(lachat_formatted)

    })


    # add visual separator between data and preview of data to upload
    output$lachatPreviewDivider <- shiny::renderUI({

      shiny::req(input$lachat_file)

      shiny::tagList(
        shiny::hr(),
        shiny::p(
          "preview data to upload",
          style = "text-align: left; background-color: LightGray; color: black;"
        )
      )

    })


    # render LACHAT file upload and interactive data fields
    output$results_reactive <- DT::renderDataTable({

      # seedDate <- if (useDateSeed()) isolate(input$resinDateSeed) else NULL

      # raw_reactive() |>
      # dplyr::mutate(
      #   omit = shinyInput(
      #     checkboxInput,
      #     nrow(raw_reactive()),
      #     "omit_",
      #     value = FALSE,
      #     width = "20px"
      #     ),
      #   newFieldID  = shinyInput(
      #     selectInput,
      #     nrow(raw_reactive()),
      #     "newFieldID_",
      #     choices = resinSamplesFrame$resinSamples,
      #     selected = NULL,
      #     multiple = FALSE,
      #     width = "120px"
      #     ),
      #   collectionDate = shinyInput(
      #     textInput,
      #     nrow(raw_reactive()),
      #     "collectionDate_",
      #     # value = NULL,
      #     value = seedDate,
      #     width = "120px",
      #     placeholder = "yyyy-mm-dd"
      #     ),
      #   notes = shinyInput(
      #     textInput,
      #     nrow(raw_reactive()),
      #     "notes_",
      #     value = NULL,
      #     width = "120px"
      #   )
      #   ) |>
      # dplyr::select(
      #   notes,
      #   collectionDate,
      #   omit,
      #   newFieldID,
      #   fieldID,
      #   everything()
      #   ) |>
      # dplyr::filter(grepl("unknown", `Sample Type`, ignore.case = TRUE))

      raw_reactive() |>
      dplyr::mutate(
        omit = shinyInputOther(
          FUN   = checkboxInput,
          len   = nrow(raw_reactive()),
          id    = paste0(session$ns("omit_")),
          value = FALSE,
          width = "20px"
          ),
        newFieldID = shinyInputOther(
          FUN      = selectInput,
          len      = nrow(raw_reactive()),
          id       = paste0(session$ns("newFieldID_")),
          choices  = resinSamplesFrame$resinSamples,
          selected = NULL,
          multiple = FALSE,
          width    = "120px"
          ),
        collectionDate = shinyInputOther(
          FUN         = textInput,
          len         = nrow(raw_reactive()),
          id          = paste0(session$ns("collectionDate_")),
          value       = Sys.Date(),
          width       = "120px",
          placeholder = "yyyy-mm-dd"
          ),
        notes = shinyInputOther(
          FUN    = textInput,
          len    = nrow(raw_reactive()),
          id     = paste0(session$ns("notes_")),
          width  = "120px"
        )
        ) |>
      dplyr::select(
        notes,
        collectionDate,
        omit,
        newFieldID,
        fieldID,
        everything()
        ) |>
      dplyr::filter(grepl("unknown", `Sample Type`, ignore.case = TRUE))

    },
    selection = "none",
    escape    = FALSE,
    server    = TRUE, # use server-side to accomodate large tables
    rownames  = FALSE,
    options   = list(
      scrollX         = TRUE,
      autoWidth       = TRUE,
      bFilter         = 0,
      bLengthChange   = FALSE,
      bPaginate       = FALSE,
      bSort           = FALSE,
      preDrawCallback = JS('function() {
        Shiny.unbindAll(this.api().table().node()); }'
        ),
      drawCallback    = JS('function() {
        Shiny.bindAll(this.api().table().node()); } '
        ),
      columnDefs      = list(
        list(
          targets = c(0),
          width   = "20px"
        )
      )
    )
    ) # close output$resultView


      # capture file upload and values provided through interactive table
      results_metadata <- reactive({

        raw_reactive() |>
        dplyr::mutate(
          omit = shinyValue(
            id  = "omit_",
            len = nrow(raw_reactive())
            ),
          newFieldID = shinyValue(
            id  = "newFieldID_",
            len = nrow(raw_reactive())
            ),
          collectionDate = shinyValue(
            id  = "collectionDate_",
            len = nrow(raw_reactive())
            ),
          notes = shinyValue(
            id  = "notes_",
            len = nrow(raw_reactive())
          )
          ) |>
        dplyr::mutate(
          newFieldID = as.character(newFieldID),
          fieldID = dplyr::case_when(
            grepl("unknown", `Sample Type`, ignore.case = TRUE) & grepl("blk", `Sample ID`, ignore.case = TRUE) ~ toupper(`Sample ID`),
            TRUE ~ fieldID
          )
        )

      })

      # preview results_metadata before upload
      output$results_metadata_preview <- DT::renderDataTable({

        results_metadata() |>
        dplyr::filter(
          grepl("unknown", `Sample Type`, ignore.case = TRUE),
          omit == FALSE
          ) |>
        dplyr::mutate(
          newFieldID = replace(newFieldID, newFieldID == "NULL", NA),
          fieldID    = case_when(
            !is.na(newFieldID) ~ newFieldID,
            TRUE ~ fieldID
          )
          ) |>
        dplyr::select(
          notes,
          collectionDate,
          fieldID,
          `Sample ID`,
          `Sample Type`,
          `Cup Number`,
          `Analyte Name`,
          `Peak Concentration`
        )
      },
      selection = "none",
      escape    = FALSE,
      server    = FALSE,
      rownames  = FALSE,
      options   = list(
        bFilter       = 0,
        bLengthChange = FALSE,
        bPaginate     = FALSE,
        bSort         = FALSE
      )
      ) # close results_metadata_preview


    # write data to database --------------------------------------------------

      shiny::observeEvent(input$lachatUpload, {

        # set tryCatch to fail if there are invalid date types; other checks are
        # embedded within the tryCatch
        tryCatch({

          lachatToUpload <- results_metadata() %>%
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

            showNotification(
              ui = "at least one unknown missing fieldID, collection date, or flag to omit",
              duration = NULL,
              closeButton = TRUE,
              type = "error")

            # 2. check for duplicates, combination of combination of fieldID,
            # collectionDate, Analyte Name, omit must be unique
          } else if (

            anyDuplicated(
              lachatToUpload[
                grepl("unknown", lachatToUpload$`Sample Type`, ignore.case = TRUE) &
                  !grepl("blk", lachatToUpload$`Sample ID`, ignore.case = T) &
                  lachatToUpload$`Sample ID` == FALSE,
                c("fieldID", "collectionDate", "Analyte Name")
                ]
            )

            ) {

            showNotification(
              ui = "at least one duplicate: fieldID x collectionDate x Analyte Name x omit",
              duration = NULL,
              closeButton = TRUE,
              type = "error")

            # 4. call data_upload, which also has a tryCatch, if all checks passed
          } else {

            data_upload(lachatToUpload)

          } # close data validation and call to upload

        }, error = function(err) {

          showNotification(
            ui = "at least one collection date is not in expected date format",
            duration = NULL,
            closeButton = TRUE,
            type = "error")

        }) # close tryCatch

      }) # close module server
} # close module function
