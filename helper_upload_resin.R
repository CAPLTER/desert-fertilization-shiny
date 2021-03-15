#' @title helper: upload_resin
#'
#' @description Function to upload resin-related Lachat data and associated
#'   metadata. This function is called at the point where the raw Lachat data
#'   have been loaded into R then married to metadata (site, date, etc.) either
#'   by manually adding that information or from a merge from metadata in a
#'   spreadsheet (or both). The workflow keeps track of upload iterations with a
#'   batch_upload. This function gets the current batch_upload and increments it
#'   by one.


# insert statement --------------------------------------------------------

resinDataUploadBaseQuery <- '
INSERT INTO urbancndep.resin (
  upload_batch,
  field_id,
  collection_date,
  notes,
  sample_id,
  sample_type,
  replicate_number,
  repeat_number,
  cup_number,
  manual_dilution_factor,
  auto_dilution_factor,
  weight_units,
  weight,
  units,
  detection_date,
  detection_time,
  user_name,
  run_file_name,
  description,
  channel_number,
  analyte_name,
  peak_concentration,
  determined_conc,
  concentration_units,
  peak_area,
  peak_height,
  calibration_equation,
  retention_time,
  inject_to_peak_start,
  conc_x_adf,
  conc_x_mdf,
  conc_x_adf_x_mdf,
  sourcefile,
  omit
)
(
  SELECT
    ?uploadBatch,
    "fieldID",
    "collectionDate",
    notes,
    "Sample ID",
    "Sample Type",
    "Replicate Number",
    "Repeat Number",
    "Cup Number",
    "Manual Dilution Factor",
    "Auto Dilution Factor",
    "Weight (Units)",
    "Weight",
    "Units",
    "Detection Date",
    "Detection Time",
    "User Name",
    "Run File Name",
    "Description",
    "Channel Number",
    "Analyte Name",
    "Peak Concentration",
    "Determined Conc.",
    "Concentration Units",
    "Peak Area",
    "Peak Height",
    "Calibration equation",
    "Retention Time",
    "Inject to Peak Start",
    "Conc x ADF",
    "Conc x MDF",
    "Conc x ADF x MDF",
    "sourceFile",
    omit
  FROM
    urbancndep.resin_temp
);'


  # main function -----------------------------------------------------------

  data_upload <- function(dataToUpload) {

    # establish db connection
    pg <- database_connection()

    # get current batch number and advance
    batchUploadCurrent <- dbGetQuery(pg, 'SELECT MAX(upload_batch) AS max FROM urbancndep.resin;')
    batchUploadNext <- as.numeric(batchUploadCurrent$max) + 1

    tryCatch({

      dbGetQuery(pg, "BEGIN TRANSACTION")

      # write new samples to resin_temp
      if (dbExistsTable(pg, c('urbancndep', 'resin_temp'))) {
        dbRemoveTable(pg, c('urbancndep', 'resin_temp'))
      }

      dbWriteTable(pg, c('urbancndep', 'resin_temp'),
                   value = dataToUpload,
                   row.names = F)

      # build the query, including next batch number
      resinDataUploadQuery <- sqlInterpolate(ANSI(),
                                             resinDataUploadBaseQuery,
                                             uploadBatch = batchUploadNext)

      # execute insert query
      dbExecute(pg,
                resinDataUploadQuery)

      # clean up
      dbRemoveTable(pg, c('urbancndep', 'resin_temp'))

      dbCommit(pg)

      showNotification(ui = "successfully uploaded",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'message',
                       action = a(href = "javascript:location.reload();", "reload the page"))

    }, warning = function(warn) {

      showNotification(ui = paste("there is a warning:", warn),
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'warning')

      print(paste("WARNING:", warn))

    }, error = function(err) {

      showNotification(ui = paste("there was an error:", err),
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error')

      print(paste("ERROR:", err))
      print("ROLLING BACK TRANSACTION")

      dbRollback(pg)

    }) # close try catch

    # close database connection
    dbDisconnect(pg)

  } # close function
