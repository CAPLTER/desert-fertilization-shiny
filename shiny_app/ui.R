ui <- shiny::tagList(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

  shiny::navbarPage(
    title = "desert fertilization",
    id    = "tabs", # use explicit id to access tab position

    shiny::tabPanel(
      title = "fertilizer",
      fertilizerUI("fertilizer")
      ),

    shiny::tabPanel(
      title = "resin: upload",
      upload_resinUI("upload_resin")
      ),

    shiny::tabPanel(
      title = "resin: data",
      ResinViewer1$ui()
      ),

    shiny::tabPanel(
      title = "annuals cover",
      cover_events_UI("annuals_cover")
    )

  ) # close navbarPage
)   # close tagList
