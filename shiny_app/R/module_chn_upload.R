#' @title module: upload CHN plant-tissue data
#'
#' @description The module \code{upload_chn} facilitates uploading resin
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

upload_chnUI <- function(id) {

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

          shiny::br(),
          shiny::br(),

          shiny::wellPanel(
            style = "background: #C9DFEC",

            shiny::helpText(
              "optional: apply an initial date to all samples",
              style = "text-align: left; color: DarkBlue;"
              ),
            shiny::selectInput(
              inputId   = ns("tissue_type"),
              label     = "tissue type",
              choices   = c("larrea", "pectocarya"),
              selected  = NULL,
              multiple  = FALSE,
              selectize = FALSE
              ),
            shiny::numericInput(
              inputId = ns("survey_year"),
              label   = "survey year",
              value   = as.integer(lubridate::year(Sys.Date())),
              min     = 2015,
              max     = 2035,
              step    = 1,
              width   = NULL
              ),
            shiny::selectInput(
              inputId   = ns("survey_season"),
              label     = "survey season",
              choices   = c("spring", "fall"),
              selected  = NULL,
              multiple  = FALSE,
              selectize = FALSE
              ),
            shiny::helpText(
              "date range of selected sampling",
              style = "text-align: left; color: DarkBlue;"
              ),
            shiny::textOutput(outputId = ns("date_range")),
            shiny::br(),
            shiny::helpText(
              "import data file contents",
              style = "text-align: left; color: DarkBlue;"
              ),
            shiny::fileInput(
              inputId  = ns("data_file"),
              label    = NULL,
              multiple = FALSE,
              accept   = c("text/csv")
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
          uiOutput(ns("preview_divider")),
          DT::dataTableOutput(ns("results_metadata_preview"))
        ) # close the right col

      ) # close row
    )   # close page
  )     # close taglist

} # close upload_chnUI


# upload main -----------------------------------------------------------------

upload_chn <- function(id, tab = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

    # set up

    # helper function for reading input functions; for reasons that are not
    # clear, this function only works if included in the .R file from which it
    # is called.
    shinyValue <- function(id, len) {
      unlist(lapply(seq_len(len), function(i) {
          value = input[[paste0(id, i)]]
          if (is.null(value)) NA else value
      }))
    }


    # display fields for rendered tables

    display_fields <- c(
      "omit",
      "plot_id",
      "collection_date",
      "tissue_type",
      "Run",
      "Run #",
      "Weight",
      "Created on",
      "Mode",
      "Comment",
      "Carbon %",
      "Hydrogen %",
      "Nitrogen %",
      "Seconds",
      "Messages",
      "sourcefile"
    )


    # query sample dates

    date_selection <- shiny::reactive({

      shiny::req(
        input$tissue_type,
        input$survey_year,
        input$survey_season
      )

      if (grepl("larrea", input$tissue_type, ignore.case = TRUE)) {

        collection_dates <- query_stems_dates(
          survey_year   = input$survey_year,
          survey_season = input$survey_season
        )

      } else if (grepl("pectocarya", input$tissue_type, ignore.case = TRUE)) {

        collection_dates <- query_annuals_dates(
          survey_year = input$survey_year
        )

      }

      return(collection_dates)

    })


    # min and max sample dates

    output$date_range <- shiny::renderText({

      paste0(
        as.character(min(date_selection()[["collection_date"]]), na.rm = TRUE),
        " to ",
        as.character(max(date_selection()[["collection_date"]]), na.rm = TRUE)
      )

    })



    # lachat data from file upload

    raw_reactive <- shiny::reactive({

      shiny::req(input$data_file)

      file_type <- tools::file_ext(input$data_file$datapath)

      if (grepl("csv", file_type)) {

        data_import <- readr::read_csv(
          file      = input$data_file$datapath,
          col_types = readr::cols(`Created on` = "c"),
          skip      = 1
        )

      } else {

        shiny::showNotification(
          ui          = "file must be of type csv",
          duration    = NULL,
          closeButton = TRUE,
          type        = "error",
          action      = a(href = "javascript:location.reload();", "reload the page")
        )

        stop()

      }


      ## check integrity of imported data

      ### expected columns

      # need to be simultaneously loose and rigorous with this check as the
      # column designations vary among runs so we need to confirm that expected
      # columns are included and at the expected location, then in
      # \code{format_chn} to ensure consitent naming of those columns

      if (
        !all(
          grepl("weight",   colnames(data_import)[3],  ignore.case = TRUE),
          grepl("comment",  colnames(data_import)[6],  ignore.case = TRUE),
          grepl("carbon",   colnames(data_import)[7],  ignore.case = TRUE),
          grepl("hydrogen", colnames(data_import)[8],  ignore.case = TRUE),
          grepl("nitrogen", colnames(data_import)[9],  ignore.case = TRUE),
          grepl("carbon",   colnames(data_import)[14], ignore.case = TRUE),
          grepl("hydrogen", colnames(data_import)[15], ignore.case = TRUE),
          grepl("nitrogen", colnames(data_import)[16], ignore.case = TRUE)
        )
        ) {

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

      expected_mode_types <- c("CHN")
      imported_mode_types <- unique(data_import[["Mode"]])

      if (!all(imported_mode_types %in% expected_mode_types)) {

        shiny::showNotification(
          ui          = "unexpected data types in imported file",
          duration    = NULL,
          closeButton = TRUE,
          type        = "error",
          action      = a(href = "javascript:location.reload();", "reload the page")
        )

        stop()

      }

      ## format data

      data_formatted <- format_chn(
        unformatted_data = data_import,
        file_name        = input$data_file$name
      )

      ## join collection date and add tissue type

      data_formatted <- data_formatted |>
      dplyr::left_join(
        date_selection(),
        by = c("plot_id")
        ) |>
      dplyr::mutate(
        tissue_type = NA_character_,
        tissue_type = dplyr::case_when(
          !is.na(plot_id) ~ input$tissue_type,
          TRUE ~ tissue_type
        )
      )

      ## return

      return(data_formatted)

    })


    # add visual separator between data and preview of data to upload
    output$preview_divider <- shiny::renderUI({

      shiny::req(input$data_file)

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

      raw_reactive() |>
      dplyr::mutate(
        omit = shinyInputOther(
          FUN   = checkboxInput,
          len   = nrow(raw_reactive()),
          id    = paste0(session$ns("omit_")),
          value = FALSE,
          width = "20px"
          ),
        ) |>
      dplyr::select(tidyselect::any_of(display_fields))

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
          targets = c(7, 8, 13, 14),
          render  = JS("$.fn.dataTable.render.ellipsis( 18 )")
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
        Comment = gsub(",", ";", Comment),
        Comment = gsub("[\r\n]", "; ", Comment)
      )

    })

    # preview results_metadata before upload
    output$results_metadata_preview <- DT::renderDataTable({

      results_metadata() |>
      dplyr::filter(
        omit == FALSE,
        !is.na(plot_id)
        ) |>
      dplyr::select(
        tidyselect::any_of(display_fields),
        -omit
      )

    },
    class      = "compact",
    plugins    = c("ellipsis"),
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
      searching       = FALSE,
      columnDefs      = list(
        list(
          targets = c(6, 8, 13, 14),
          render  = JS("$.fn.dataTable.render.ellipsis( 18 )")
        )
      )
    )
    ) # close results_metadata_preview


    # write data to database --------------------------------------------------

    shiny::observeEvent(input$upload_data, {

      ## validate sample ids, dates, etc.

      samples_only <- results_metadata() |>
      dplyr::filter(
        omit == FALSE,
        !is.na(plot_id)
      )

      sample_ids_message <- c()

      if (any(is.na(samples_only[, "collection_date"]))) { 

        sample_ids_message <- append(
          x      = sample_ids_message,
          values = "at least one sample missing collection_date or flag to omit"
        )

      }

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

    # observe(readr::write_csv({ results_metadata() }, "/tmp/results_metadata.csv"))
    # observe(readr::write_csv({ resultReactive() }, "/tmp/lachat_results_reactive.csv"))
    # observe(print({ date_selection() }))
    # observe(print({ colnames(raw_reactive()) }))
    # observe(print({ dplyr::glimpse(results_metadata()) }))


    # close module -----------------------------------------------------------

}) # close module server
}  # close module function
