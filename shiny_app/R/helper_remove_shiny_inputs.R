#' @title helper: remove shiny inputs (to modules)
#'
#' @description A helper function to remove used shiny module inputs; called
#' when selecting a event.
#'
#' @note Critical is to provide the exact name of the module id for which
#' associated inputs should be removed. A helpful way to get that id if there
#' is any doubt, is to add  something along the lines of message("my moudle ",
#' session$ns(id)) in the module that is being called. 
#'
#' @note See the following post for details:
#' https://roh.engineering/posts/2020/02/shiny-add/removing-modules-dynamically/
#'
#' @export
#'
remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(
      grep(id, names(.input), value = TRUE),
      function(i) {
        .subset2(.input, "impl")$.values$remove(i)
      }
    )
  )
}
