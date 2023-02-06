#' @title helper: pre-process resin data for validation and upload
#'
#' @description Resin data require certain formatting prior to validating and
#' uploading. As there are commonalities between formatting required for both
#' validation and upload, those steps are combined in a function that addresses
#' these steps for next validation then upload if the checks pass.
#'
#' @export

prepare_resin_data <- function(data_to_prepare) { 

  prepped_data <- data_to_prepare |>
  dplyr::mutate(
    notes           = replace(notes, notes == "", NA),
    field_id_rev    = replace(field_id_rev, field_id_rev == "NULL", NA),
    field_id        = dplyr::case_when(
      !is.na(field_id_rev) ~ field_id_rev,
      TRUE ~ field_id
      ),
    collection_date = replace(collection_date, collection_date == "", NA),
    collection_date = dplyr::case_when(
      grepl("blk", field_id, ignore.case = TRUE) ~ as.Date(NA),
      TRUE ~ as.Date(collection_date)
    )
  )

  return(prepped_data)

}
