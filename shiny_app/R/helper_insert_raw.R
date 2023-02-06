#' @title helper: build queries for inserting RAW analysis (ICP, AQ2, Lachat,
#' Shimadzu) output
#'
#' @description The function \code{build_insert_raw_query} constructs a query
#' to insert raw machine (ICP, AQ2, Lachat, Shimadzu) output into the
#' appropriate database table.
#'
#' @export

build_insert_raw_query <- function(currentTab) {

  # chn ------------------------------------------------------------------------

  if (grepl("chn", currentTab, ignore.case = TRUE)) {

    parameterized_query <- glue::glue_sql('
      INSERT INTO urbancndep.plant_tissue_chn
      (
        "Run",
        "Run #",
        "Weight",
        "Created on",
        "Mode",
        "Comment",
        "Carbon %",
        "Hydrogen %",
        "Nitrogen %",
        "ZR",
        "CR",
        "HR",
        "NR",
        "Carbon",
        "Hydrogen",
        "Nitrogen",
        "Seconds",
        "Messages",
        plot_id,
        collection_date,
        tissue_type,
        source_file,
        upload_batch,
        omit
      )
      (
        SELECT
        "Run",
        "Run #",
        "Weight",
        "Created on",
        "Mode",
        "Comment",
        "Carbon %",
        "Hydrogen %",
        "Nitrogen %",
        "ZR",
        "CR",
        "HR",
        "NR",
        "Carbon",
        "Hydrogen",
        "Nitrogen",
        "Seconds",
        "Messages",
        plot_id,
        collection_date,
        tissue_type,
        sourcefile,
        upload_batch,
        omit
        FROM urbancndep.temp_raw
      )
      ;
      ',
      .con = DBI::ANSI()
    )

    # resin --------------------------------------------------------------------

  } else if (grepl("resin", currentTab, ignore.case = TRUE)) {

    parameterized_query <- glue::glue_sql("
      INSERT INTO urbancndep.resin
      (
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
        omit,
        sourcefile
      )
      (
        SELECT
        upload_batch,
        field_id,
        collection_date,
        NULLIF(notes, 'NA')::text,
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
        omit,
        sourcefile
        FROM urbancndep.temp_raw
      )
      ;
      ",
      .con = DBI::ANSI()
    )

  } else {

    parameterized_query <- NULL

  }

  return(parameterized_query)

}
