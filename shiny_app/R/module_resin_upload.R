#' @title module: upload Lachat resin data
#'
#' @description The module \code{upload_resin} facilitates uploading resin
#' Lachat data. The user attaches the appropriate sample details to uploaded
#' data. Upon execution, the munged data with sample and analysis details are
#' written to 'urbancndep.resin'.
#'
#' @note The setup is a bit unusual in that we are using a text field input to
#' facilitate adding the collection date. The reason for this is that the
#' `shinyInputOther` function, while it will accept a shiny::dateInput as an
#' input type, shiny::dateInput defaults to Sys.Date() as the NULL value. That
#' is, shiny::dateInput cannot have a NULL value. This creates a problem for
#' the dynamic data entry of the type addressed here where we attach and/or
#' edit data associated with a table in that we need for those input fields to
#' start as NULL or empty. The workaround is to enter collection date as a
#' shiny::inputText, which can have a NULL or empty default value.
#'
#' @note Using here scroller for the first time when rendering the interactive
#' dataTable. The parameter `scrollY` sets the height of the rendered table.
#' This parameter does not have to be set but the logic that Shiny and/or
#' dataTables uses to dynamically set the height of the table if this is not
#' set does not seem to work well. As such, this parameter should be set.
#' Initial value in this instance is set to 800, which can and should be
#' adjusted based on user feedback. `bPaginate` (TRUE) is required but it is
#' not clear the argument provided to `pageLength` has any bearing on how the
#' table is rendered, or that `bLengthChange` has any bearing on the display or
#' funcationality of the table.
#'
#' @note Tried the KeyTable extension, which provides Excel-like navigation
#' around a table with keys, which would have been very nice for navigating
#' these large tables especially since the scrollbars in dataTables are clunky.
#' Unfortunately, KeyTable had the hindering effect of impeding copying and
#' pasting within the table. That is, when, for example, copying a data field
#' from one input and attempting to paste into another row, with KeyTable
#' enabled, what you actually copy is the html for that cell of the table
#' rather than the content of the cell. KeyTable is very but probably not
#' useable in dynamic tables.
#'
#'
#' @export

# upload UI -------------------------------------------------------------------

upload_resinUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidPage(

      shiny::fluidRow(
        shiny::column(
          id = "readme_row",
          width = 12,
          shiny::div(
            id = "readme_box",
            shiny::strong("README"),
            "All resin data samples must have an associated collection date. For convenience, an optional default collection date can be applied to all samples when the resin data Lachat file is imported by providing a valid date in the default date entry field - this will apply the date entered as the collection date for all samples in the imported data. Dates for samples that were not collected on the provided default date should be edited as appropriate in the collection_date column. Note that providing a default date is not required. It must be supplied in advance of importing the data. Use field_id_rev to assign sample ids to samples with missing or incorrect sample_ids. When all notes, dates, and field ids have been applied and any samples not intended for upload (omit) have been highlighted, etc., use the submit data button to upload data to the database."
          ) # close readme div
        )   # close readme column
        ),  # close readme row

      shiny::fluidRow(
        shiny::column(
          id    = "left_panel",
          width = 2,

          shiny::wellPanel(
            style = "background: #C9DFEC",

            shiny::helpText(
              "optional: apply an initial date to all samples",
              style = "text-align: left; color: DarkBlue;"
              ),
            shiny::textInput(
              inputId     = ns("approximate_date"),
              label       = NULL,
              value       = NULL,
              placeholder = "yyyy-mm-dd"
              ),
            shiny::br(),
            shiny::helpText(
              "import Lachat file contents",
              style = "text-align: left; color: DarkBlue;"
              ),
            shiny::fileInput(
              inputId  = ns("lachat_file"),
              label    = NULL,
              multiple = FALSE,
              accept   = c(
                "text/csv",
                "application/vnd.ms-excel",
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
              )
              ),
            shiny::br(),
            shiny::actionButton(
              inputId = ns("upload_data"),
              label   = "upload data",
              class   = "btn-success",
              style   = "color: #fff;",
              icon    = shiny::icon("plus"),
              width   = "100%"
              ),
            shiny::br()
          )  # close well panel
          ), # close left col

        shiny::column(
          id    = "right_panel",
          width = 10,
          DT::dataTableOutput(ns("results_reactive")),
          uiOutput(ns("lachatPreviewDivider")),
          DT::dataTableOutput(ns("results_metadata_preview"))
        ) # close the right col

      ) # close row
    )   # close page
  )     # close taglist

} # close upload_resinUI


# upload main -----------------------------------------------------------------

upload_resin <- function(id, tab = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

    # helper function for reading input functions; for reasons that are not
    # clear, this function only works if included in the .R file from which it
    # is called.
    shinyValue <- function(id, len) {
      unlist(lapply(seq_len(len), function(i) {
          value = input[[paste0(id, i)]]
          if (is.null(value)) NA else value
      }))
    }


    # use an approximate starting date; this is optional but must be supplied
    # before loading data if enabled
    approximate_date_reactive <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(
      eventExpr  = input$approximate_date,
      ignoreInit = FALSE,
      once       = FALSE, {

        approximate_date_reactive(TRUE)

      })


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


      ## check integrity of imported data

      ### expected columns

      expected_columns <- c("Sample ID","Sample Type","Replicate Number","Repeat Number","Cup Number","Manual Dilution Factor","Auto Dilution Factor","Weight (Units)","Weight","Units","Detection Date","Detection Time","User Name","Run File Name","Description","Channel Number","Analyte Name","Peak Concentration","Determined Conc.","Concentration Units","Peak Area","Peak Height","Calibration equation","Retention Time","Inject to Peak Start","Conc x ADF","Conc x MDF","Conc x ADF x MDF")

      if (!identical(colnames(lachat_import), expected_columns)) {

        shiny::showNotification(
          ui          = "unexpected columns in imported file",
          duration    = NULL,
          closeButton = TRUE,
          type        = "error",
          action      = a(href = "javascript:location.reload();", "reload the page")
        )

        stop()

      }

      ### expected sample types (rows)

      expected_sample_types <- c("Calibration Standard","Method Detection Limit","Check Standard","Unknown")
      imported_sample_types <- unique(lachat_import[["Sample Type"]])

      if (!all(imported_sample_types %in% expected_sample_types)) {

        shiny::showNotification(
          ui          = "unexpected sample types in imported file",
          duration    = NULL,
          closeButton = TRUE,
          type        = "error",
          action      = a(href = "javascript:location.reload();", "reload the page")
        )

        stop()

      }


      ## format lachat data

      lachat_formatted <- format_lachat(
        lachat_data = lachat_import,
        file_name   = input$lachat_file$name
      )

      ## join imported data with samples list to pre-populate sample id

      lachat_formatted <- lachat_formatted |>
      dplyr::left_join(
        resinSamplesFrame,
        by = c("sample_id" = "resinSamples")
        ) |>
      dplyr::rename(field_id = databaseID)

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

      if (approximate_date_reactive()) {

        approximate_collection_date <- shiny::isolate(input$approximate_date)
        approximate_collection_date <- as.character(as.Date(approximate_collection_date))

      } else {

        approximate_collection_date <- NULL

      }

      raw_reactive() |>
      dplyr::mutate(
        omit = shinyInputOther(
          FUN   = checkboxInput,
          len   = nrow(raw_reactive()),
          id    = paste0(session$ns("omit_")),
          value = FALSE,
          width = "20px"
          ),
        field_id_rev = shinyInputOther(
          FUN      = selectInput,
          len      = nrow(raw_reactive()),
          id       = paste0(session$ns("newFieldID_")),
          choices  = resinSamplesFrame$resinSamples,
          selected = NULL,
          multiple = FALSE,
          width    = "120px"
          ),
        collection_date = shinyInputOther(
          FUN         = textInput,
          len         = nrow(raw_reactive()),
          id          = paste0(session$ns("collectionDate_")),
          value       = approximate_collection_date,
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
        collection_date,
        omit,
        field_id_rev,
        field_id,
        sample_id,
        sample_type,
        cup_number,
        manual_dilution_factor,
        auto_dilution_factor,
        detection_date,
        detection_time,
        channel_number,
        analyte_name,
        peak_concentration,
        conc_x_adf_x_mdf,
        sourcefile
        ) |>
      dplyr::filter(grepl("unknown", sample_type, ignore.case = TRUE))

    },
    class      = "cell-border stripe",
    plugins    = c("ellipsis"),
    extensions = c("Scroller"),
    selection  = "none",
    escape     = FALSE,
    server     = TRUE, # server-side ~ accomodate large tables
    rownames   = FALSE,
    options    = list(
      bPaginate       = TRUE,   # scroller
      pageLength      = 50,     # scroller
      deferRender     = FALSE,  # scroller
      scrollY         = 800,    # scroller
      scroller        = TRUE,   # scroller
      autoWidth       = FALSE,
      scrollX         = TRUE,
      bLengthChange   = FALSE,
      bSort           = FALSE,
      bFilter         = 0,
      fixedHeader     = FALSE,
      searching       = FALSE,
      preDrawCallback = JS('function() {
        Shiny.unbindAll(this.api().table().node()); }'
        ),
      drawCallback    = JS('function() {
        Shiny.bindAll(this.api().table().node()); } '
        ),
      columnDefs      = list(
        list(
          targets = c(16),
          render  = JS("$.fn.dataTable.render.ellipsis( 30 )")
        )
      )
    )
    ) # close output results_reactive


    # capture file upload and values provided through interactive table
    results_metadata <- reactive({

      raw_reactive() |>
      dplyr::mutate(
        omit = shinyValue(
          id  = "omit_",
          len = nrow(raw_reactive())
          ),
        field_id_rev = shinyValue(
          id  = "newFieldID_",
          len = nrow(raw_reactive())
          ),
        collection_date = shinyValue(
          id  = "collectionDate_",
          len = nrow(raw_reactive())
          ),
        notes = shinyValue(
          id  = "notes_",
          len = nrow(raw_reactive())
        )
        ) |>
      dplyr::mutate(
        field_id_rev = as.character(field_id_rev),
        field_id = dplyr::case_when(
          grepl("unknown", sample_type, ignore.case = TRUE) & grepl("blk", sample_id, ignore.case = TRUE) ~ toupper(sample_id),
          TRUE ~ field_id
          ),
        notes = gsub(",", ";", notes),
        notes = gsub("[\r\n]", "; ", notes)
      )

    })

    # preview results_metadata before upload
    output$results_metadata_preview <- DT::renderDataTable({

      results_metadata() |>
      dplyr::filter(
        grepl("unknown", sample_type, ignore.case = TRUE),
        omit == FALSE # filter omits in view but not data
        ) |>
      dplyr::mutate(
        field_id_rev = replace(field_id_rev, field_id_rev == "NULL", NA),
        field_id     = dplyr::case_when(
          !is.na(field_id_rev) ~ field_id_rev,
          TRUE ~ field_id
        )
        ) |>
      dplyr::select(
        notes,
        collection_date,
        field_id,
        sample_id,
        sample_type,
        cup_number,
        analyte_name,
        conc_x_adf_x_mdf
      )
    },
    class      = "compact",
    extensions = c("Scroller"),
    selection  = "none",
    escape     = FALSE,
    server     = FALSE, # server-side ~ accomodate large tables
    rownames   = FALSE,
    options    = list(
      bPaginate       = TRUE,   # scroller
      pageLength      = 5,      # scroller
      deferRender     = FALSE,  # scroller
      scrollY         = 800,    # scroller
      scroller        = TRUE,   # scroller
      autoWidth       = FALSE,
      scrollX         = TRUE,
      bLengthChange   = FALSE,
      bSort           = FALSE,
      bFilter         = 0,
      fixedHeader     = FALSE,
      searching       = FALSE
    )
    ) # close results_metadata_preview


    # write data to database --------------------------------------------------

    shiny::observeEvent(input$upload_data, {

      ## validate sample ids, dates, etc.

      sample_ids_message <- check_sample_ids(results_metadata())


      ## initiate upload if checks pass

      if (length(sample_ids_message) != 0) {

        notification_message <- paste(sample_ids_message, collapse = " & ")

        shiny::showNotification(
          ui          = notification_message,
          duration    = 8,
          closeButton = TRUE,
          type        = "error"
        )

      } else {

        upload_chemistry(
          this_raw_reactive = results_metadata(),
          this_analysis     = stringr::str_extract(tab(), "^\\w+")
        )

        remove_table(
          schema_name = "urbancndep",
          table_name  = "temp_raw"
        )

      }

    }) # close submit data


    # debugging: module level ------------------------------------------------

    # observe(readr::write_csv({ rawReactive() }, "/tmp/lachat_raw.csv"))
    # observe(readr::write_csv({ resultReactive() }, "/tmp/lachat_results_reactive.csv"))
    # observe(print({ tab() }))
    # observe(print({ colnames(raw_reactive()) }))
    # observe(print({ dplyr::glimpse(raw_reactive()) }))


    # close module -----------------------------------------------------------

}) # close module server
}  # close module function
