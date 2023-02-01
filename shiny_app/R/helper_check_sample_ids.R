#' @title helper: validate that all lachat resin data are identified
#' appropriately 
#'
#' @description The function \code{check_sample_ids} tests whether all of the
#' unknown samples in a file of lachat resin data have been associated with a
#' field_id, and that there are not any duplicates. Returns FALSE if all tests
#' have passed, or a message indicating which test(s) failed.
#'
#' @export

check_sample_ids <- function(data_to_validate) { 

  ## filter and check only unknowns

  data_subset <- prepare_resin_data(data_to_validate) |>
  dplyr::filter(omit == FALSE)


  ## setup

  validation_message <- c()


  ## check: non-blank samples not flagged to omit are missing collection_date
  ## and not in the future

  non_blanks <- data_subset |>
  dplyr::filter(!grepl("blk", field_id, ignore.case = T))


  if (max(non_blanks[["collection_date"]]) > Sys.Date()) { 

    validation_message <- append(
      x      = validation_message,
      values = "at least one sample has a collection_date in the future"
    )

  }

  if (any(is.na(non_blanks[, "collection_date"]))) { 

    validation_message <- append(
      x      = validation_message,
      values = "at least one sample missing collection_date or flag to omit"
    )

  }


  ## check: any samples not flagged to omit are missing a field_id

  if (any(is.na(data_subset[, "field_id"]))) { 

    validation_message <- append(
      x      = validation_message,
      values = "at least one sample missing field_id or flag to omit"
    )

  }


  ## check: duplicate combinations of field_id x collection_date x analyte

  id_fields <- c("field_id", "collection_date", "analyte_name")

  sample_ids_only <- data_subset |>
  dplyr::select(tidyselect::any_of(id_fields))

  if (anyDuplicated(sample_ids_only)) {

    validation_message <- append(
      x      = validation_message,
      values = "at least one duplicate field_id x collection_date x omit"
    )

  }


  ## return

  return(validation_message)

}
