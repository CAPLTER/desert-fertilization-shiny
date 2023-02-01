#' @title helper: format raw lachat data for 
#'
#' @description Function to format resin-related Lachat data. Inputs include
#' the Lachat data (R object) and name of file from which the Lachat data were
#' derived. As much as possible, the Sample ID field (basically the identify of
#' the collection site (e.g., LDP) and type (e.g., IP)) is standardized based
#' on observed patterns to facilitate a match between the collector id entered
#' as metadata (ideally the Lachat Sample ID for the unknown samples is from
#' bar code so always the same (but, yeah, right)). The purpose of the
#' formatting is to standardize the Lachat for upload and to facilitate
#' marrying the Sample ID to the list of defined Sample IDs generated in
#' helper_list_resin_samples.
#'
#' @export
#'

format_lachat <- function(
  lachat_data,
  file_name
  ) {

  # format column names - moved to janitor
  # colnames(lachat_data) <- tolower(colnames(lachat_data))             # colnames to lowercase
  # colnames(lachat_data) <- gsub("\\.", "\\_", colnames(lachat_data))  # replace dots with underscores
  # colnames(lachat_data) <- gsub(" ", "\\_", colnames(lachat_data))    # replace spaces with underscores

  # format data in columns
  lachat_data_fmt <- lachat_data |>
  janitor::clean_names() |>
  dplyr::mutate(
    sample_id      = gsub("\\.$", "", sample_id),                 # remove any trailing dots
    sample_id      = gsub("LATR.CNTL", "LATR CNTL", sample_id),   # remove intermittent "." between LATR & CTRL
    sample_id      = gsub("IP.CNTL", "IP CNTL", sample_id),       # remove intermittent "." between IP & CTRL
    sample_id      = gsub("LATR\\.", "LATR", sample_id),          # remove intermittent "." between LATR & replicate no.
    sample_id      = gsub("(LATR)(CNTL)", "\\1 \\2", sample_id),  # ensure space between LATR & CNTL
    detection_date = as.character(detection_date),
    detection_time = as.character(detection_time, format = "%H:%M:%S"),
    # idToJoin       = sample_id,                                   # duplicate Sample ID for joining
    sourcefile     = basename(file_name)                          # add name of source data
  )

  return(lachat_data_fmt)

}
