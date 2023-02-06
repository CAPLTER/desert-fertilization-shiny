#' @title helper: pre-process CHN data for upload
#'
#' @description CHN data require certain formatting prior to validating and
#' uploading. As there are commonalities between formatting required for both
#' validation and upload, those steps are combined in a function that addresses
#' these steps for next validation then upload if the checks pass.
#'
#' @export

prepare_chn_data <- function(data_to_prepare) { 

  ## formalize plant names

  prepped_data <- data_to_prepare |>
  dplyr::mutate(
    tissue_type = dplyr::case_when(
      grepl("larrea", tissue_type, ignore.case = TRUE) ~ "Larrea tridentata",
      grepl("pectocarya", tissue_type, ignore.case = TRUE) ~ "Pectocarya recurvata",
      TRUE ~ tissue_type
    )
  )

  return(prepped_data)

}
