server <- function(input, output, session) {

  # establish tab position as input to modules --------------------------------

  tabID <- shiny::reactive({ input$tabs })


  # listeners

  listener_init("update_composition")
  listener_init("update_annuals_biomass")


  # modules -------------------------------------------------------------------

  fertilizer("fertilizer")
  upload_resin("upload_resin", tab = tabID)
  upload_chn("upload_chn", tab = tabID)
  chemistry_data("chemistry_data")
  cover_events("cover_events")
  annuals_biomass("annuals_biomass")

  # debugging -----------------------------------------------------------------

  # observe(print({ mergedWithAnnotations() }))
  # observe(print({ str(mergedWithAnnotations()) }))
  # observe(print({ colnames(mergedWithAnnotations()) }))
  # observe(print({ ResinViewer1 }))


} # close server
