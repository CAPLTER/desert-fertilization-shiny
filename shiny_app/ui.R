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
  navbarPage(
    "desert fertilization",

    # resin upload tab --------------------------------------------------------

    # tabPanel(
    #   title = "resin: file upload",
    #   fluidPage(
    #     fluidRow(
    #       column(
    #         id = "leftPanel", 2,
    #         br(),
    #         helpText("1. identify a default date for sample collection (optional)",
    #           style = "text-align: left; color: DarkBlue; font-weight: bold"),
    #         textInput(inputId = "resinDateSeed",
    #           label = NULL,
    #           value = NULL,
    #           placeholder = "yyyy-mm-dd"),
    #         br(),
    #         helpText("2. upload Lachat file",
    #           style = "text-align: left; color: DarkBlue; font-weight: bold"),
    #         fileInput(inputId = "file1",
    #           label = NULL,
    #           multiple = FALSE,
    #           accept = c("text/csv",
    #             "application/vnd.ms-excel",
    #             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
    #         helpText(id = "explainLachatUpload",
    #           "use upload lachat to upload annotated lachat data without merging"),
    #         actionButton("lachatUpload",
    #           "upload lachat"),
    #         br()
    #         ), # close the left col
    #       column(
    #         id = "fileUploadRightPanel", 10,
    #         # lachat annotations
    #         DT::dataTableOutput("dynamicLachat"),
    #         uiOutput("lachatPreviewDivider"),
    #         DT::dataTableOutput("dynamicLachatPreview")
    #       ) # close the right col
    #     ) # close the row
    #   ) # close the page
    #   ), # close 'resin: file upload' tab panel

    # resin viewer tab --------------------------------------------------------

    tabPanel(
      title = "resin: data viewer",
      ResinViewer1$ui()
      ),


    # fertilizer tab ----------------------------------------------------------

    shiny::tabPanel(
      title = "fertilizer",
      fertilizerUI("fertilizer")
    ),


    # annuals cover -----------------------------------------------------------

    tabPanel(
      title = "annuals cover",
      cover_events_UI("annuals_cover")
    ) # close annuals cover UI

  ) # close navbar/page
) # close tagList
