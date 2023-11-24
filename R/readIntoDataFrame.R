# Function to convert exracted information from an epx-file into a data.frame
# This is part of a project to create a package to read epx-files from EpiData into R

# Johann Popp
# 2019-06-23
# Last update: 2023-11-14
###########################################


#' Helper: Reading xml-data from a EpiData-epx file into a data.frame.
#'
#' A helper function of \code{\link{read.EpiData}} to read xml-data into a data.frame
#'
#' @param x Data set informations extracted by \code{\link{epx.extract}} (info [[7]]).
#'
#' @return A data.frame
#' @export
#' @examples
#' # This demonstrates the effects of all six helper functions to \code{\link{read.EpiData}}.
#'
#' (x <- epx.example("SomeFakeData.epx"))
#'
#' # epx.extract() extracts the information from an EpiData-epx file
#' (info <- epx.extract(x))
#'
#' # epx.read() transforms the information into a data.frame
#' (dat <- lapply(info$perDataSet, epx.read))
#'
#' # Combine dat and info in a data-set-wise list
#' # This is a necessary detour to handle EpiData files with multiple data sets.
#' perDataSet <- mapply(function(dat, info) list(list(dat = dat, info = info)), dat, info[[7]])
#'
#' # epx.labels() replaces value codes with value labels
#' (datLab <- epx.labels(perDataSet$ds1$dat, perDataSet$ds1$info))
#'
#' # epx.missing() replaces definde missing values with NA
#' (datMis <- epx.missing(perDataSet$ds1$dat, perDataSet$ds1$info))
#'
#' # epx.class() sets variable classes according to the field types defined in EpiData
#' (datClass <- epx.class(perDataSet$ds1$dat, perDataSet$ds$info))
#'
#' # Bringing it all together
#' read.EpiData(epx.example("SomeFakeData.epx"))


epx.read <- function(x){
  if(length(x$records > 0)){
  records <- x$records
  fieldNames <- x$fieldNames
  fieldLabels <- x$fieldLabels
  fieldTypes <- x$fieldTypes

  # Add category "NA" for missing values in the records
  records <- paste("NA;", records, sep="")
  # Construct a pattern to seperate individual variables in the records
  pattern <- paste(";", paste(fieldNames, "=", sep = "", collapse = "|;"), sep = "")

  # Enter values from the records into a data.frame.
    dat <- as.data.frame(
      lapply(
        # colnames(dat),
        fieldNames,
        function(y){
          records2 <- gsub(paste("[[:print:]]*", ";", y, "=", sep = ""), "", records)
          unlist(lapply(strsplit(records2, pattern), function(x) x[1]))
        }),
      stringsAsFactors = FALSE, col.names = fieldNames)

  # Delete double quotation marks at the beginning and end of text fields
  characterField <- grepl("ftString|ftMemo|ftUpperString", fieldTypes)
  if(sum(characterField) == 1) {
    dat[,characterField] <-
    gsub("^\"|\"$", "", dat[,characterField])
  } else {
    # if(sum(grepl(characterPattern, fieldTypes)) > 1) {
    if(sum(characterField) >1){
      # dat[,grepl(characterPattern, fieldTypes)] <-
      dat[,characterField] <-
        apply(dat[,characterField], 2, function(x) gsub("^\"|\"$", "", x))
    }
  }

  # Include variable labels
  attributes(dat)$variable.labels <- fieldLabels


  dat} else {"This data frame has no entries."}
}

