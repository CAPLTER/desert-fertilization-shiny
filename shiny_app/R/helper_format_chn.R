#' @title helper: format raw CHN data
#'
#' @description Function to format CHN tissue data. Inputs include the CHN data
#' (R object) and name of file from which the data were derived. The purpose of
#' the formatting is to standardize the file contents for upload and to
#' facilitate marrying the Sample or Run ID to the list of defined plot IDs and
#' corresponding collection dates for the field survey year, season, and type.
#'
#' @export
#'

format_chn <- function(
  unformatted_data,
  file_name
  ) {

  ## column names

  colnames(unformatted_data)[1]  <- "Run"
  colnames(unformatted_data)[3]  <- "Weight"
  colnames(unformatted_data)[6]  <- "Comment"

  colnames(unformatted_data)[7]  <- "Carbon %"
  colnames(unformatted_data)[8]  <- "Hydrogen %"
  colnames(unformatted_data)[9]  <- "Nitrogen %"

  colnames(unformatted_data)[14] <- "Carbon"
  colnames(unformatted_data)[15] <- "Hydrogen"
  colnames(unformatted_data)[16] <- "Nitrogen"


  ## add Messages field if it does not exist in the data

  if (!any(grepl("messages", colnames(unformatted_data), ignore.case = TRUE))) {

    unformatted_data[["Messages"]] <- NA_character_

  }

  
  ## plot_id sourcefile

  data_fmt <- unformatted_data |>
  dplyr::mutate(
    plot_id      = as.integer(
      stringr::str_extract(
        Run,
        "^[0-9]+$|(?<=PLOT\\s)[0-9.]+|(?<=Plot\\s)[0-9.]+|(?<=plot\\s)[0-9.]+")
      ),
    `Created on` = as.character(`Created on`),
    sourcefile   = basename(file_name) # add name of source data
  )

  return(data_fmt)

}
