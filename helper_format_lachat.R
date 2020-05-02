
# README ------------------------------------------------------------------

# Function to format resin-related Lachat data. Inputs include the Lachat data
# (R object) and name of file from which the Lachat data were derived. As much
# as possible, the Sample ID field (basically the identify of the collection
# site (e.g., LDP) and type (e.g., IP)) is standardized based on observed
# patterns to facilitate a match between the collector id entered as metadata
# (ideally the Lachat Sample ID for the unknown samples is from bar code so
# always the same (but, yeah, right)). The purpose of the formatting is to
# standardize the Lachat for upload and to facilitate marrying the relevant
# metadata (site, collection date, notes, etc.) either by manually entering
# those details or merging them with metadata entered into an Excel file (see
# import_metadata.R).


# libraries ---------------------------------------------------------------

library(zoo)


# main function -----------------------------------------------------------

format_lachat <- function(lachatData, fileName) {
  
  lachatData <- lachatData %>% 
    mutate(
      # as much as possible, standardize Sample IDs
      # `Sample ID` = gsub("(\\w+\\.\\w+)(\\s[0-9].+)", "\\1", `Sample ID`), # remove CK's date ref
      # `Sample ID` = replace(`Sample ID`, grepl("BLK[0-9]", `Sample ID`), "BLK.BLK"), # numbered blks to blks
      `Sample ID` = gsub("\\.$", "", `Sample ID`), # remove any trailing dots
      `Sample ID` = gsub("LATR.CNTL", "LATR CNTL", `Sample ID`), # remove intermittent "." between LATR & CTRL
      `Sample ID` = gsub("IP.CNTL", "IP CNTL", `Sample ID`), # remove intermittent "." between IP & CTRL
      `Sample ID` = gsub("LATR\\.", "LATR", `Sample ID`), # remove intermittent "." between LATR & replicate no.
      `Sample ID` = gsub("(LATR)(CNTL)", "\\1 \\2", `Sample ID`), # ensure space between LATR & CNTL
      `Detection Date` = as.Date(`Detection Date`),
      `Detection Time` = as.character(`Detection Time`, format = "%H:%M:%S"),
      idToJoin = `Sample ID`, # duplicate Sample ID for joining
      sourceFile = basename(fileName) # add name of source data
    )
  
  return(lachatData)
  
}