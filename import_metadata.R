
# README ------------------------------------------------------------------

# Function to upload and format resin-related metadata from an Excel file
# worksheet. Inputs include the Excel file name and worksheet name. As much as
# possible, the collector id field (basically the identify of the collection
# site (e.g., LDP) and type (e.g., IP)) is standardized based on observed
# patterns to facilitate a match between the collector id entered in the Excel
# file and the Sample ID in the corresponding Lachat data file (ideally from bar
# code so always the same (but, yeah, right)). The purpose of the upload and
# formatting is to extract relevant metadata to pair with Lachat output.


# libraries ---------------------------------------------------------------

library(zoo)


# main function -----------------------------------------------------------

import_metadata <- function(metadataFile, worksheetName) {
  
  fieldData <- read_excel(path = metadataFile,
                          sheet = worksheetName,
                          col_types = c("text", "text", "date", "text", "text", "text", "numeric", "numeric", "text", "text", "text"))
  
  fieldData <- fieldData %>% 
    mutate(
      site = na.locf(site),
      # collectionDate = as.Date(`date:`, format = "%m/%d/%Y"),
      collectionDate = as.character(`date:`, format = "%Y-%m-%d"),
      # collectionDate = na.locf(`date:`),
      collectionDate = na.locf(collectionDate),
      # merge field and lab notes into a single notes field
      notes = ifelse(!is.na(`field note:`) & is.na(`lab notes`),`field note:`,
                     ifelse(is.na(`field note:`) & !is.na(`lab notes`), `lab notes`,
                            ifelse(!is.na(`field note:`) & !is.na(`lab notes`), paste(`field note:`, `lab notes`, sep = ";"), NA))),
      # as much as possible, standardize collecto id to match lachat sample id
      `collector id` = replace(`collector id`, `collector id` == 'LATR 1', 'LATR1'),
      `collector id` = replace(`collector id`, `collector id` == 'LATR 2', 'LATR2'),
      `collector id` = replace(`collector id`, `collector id` == 'IP 1', 'IP1'),
      `collector id` = replace(`collector id`, `collector id` == 'IP 2', 'IP2'),
      `collector id` = replace(`collector id`, `collector id` == 'Control LATR1', 'LATR CNTL1'),
      `collector id` = replace(`collector id`, `collector id` == 'Control LATR2', 'LATR CNTL2'),
      `collector id` = replace(`collector id`, `collector id` == 'Control LATR3', 'LATR CNTL3'),
      `collector id` = replace(`collector id`, `collector id` == 'Control IP1', 'IP CNTL1'),
      `collector id` = replace(`collector id`, `collector id` == 'Control IP2', 'IP CNTL2'),
      `collector id` = replace(`collector id`, `collector id` == 'Control IP3', 'IP CNTL3'),
      `collector id` = replace(`collector id`, grepl('lab blank', `collector id`, ignore.case = TRUE), 'BLK'),
      site = replace(site , grepl('Lab', site, ignore.case = TRUE), 'BLK'),
      fieldID = paste(site, `collector id`, sep = ".") # build fieldID from site and collector id
    ) %>% 
    filter(!is.na(`collector id`)) %>% 
    select(fieldID, collectionDate, notes) %>% # pare to relevant fields
    distinct(fieldID, collectionDate, notes) # distinct records only
  
  return(fieldData)
  
}