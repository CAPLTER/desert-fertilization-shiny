function annuals_biomass_module_js(ns_prefix) {

  $("#" + ns_prefix + "annuals_biomass_view").on("click", ".delete_btn", function() {
    Shiny.setInputValue(ns_prefix + "annuals_biomass_to_delete", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });

  $("#" + ns_prefix + "annuals_biomass_view").on("click", ".edit_btn", function() {
    Shiny.setInputValue(ns_prefix + "annuals_biomass_to_edit", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });

  $("#" + ns_prefix + "annuals_biomass_view").on("click", ".info_btn", function() {
    Shiny.setInputValue(ns_prefix + "annuals_biomass_to_populate", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });

}
