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
      title = "chn: upload",
      upload_chnUI("upload_chn")
      ),

    shiny::tabPanel(
      title = "chemistry data",
      chemistry_dataUI("chemistry_data")
      ),

    shiny::tabPanel(
      title = "annuals cover",
      cover_eventsUI("cover_events")
    ),

    shiny::tabPanel(
      title = "annuals biomass",
      annuals_biomassUI("annuals_biomass")
    )

  ) # close navbarPage
)   # close tagList
