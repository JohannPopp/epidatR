# Function to convert codes, that are defines as missing values into NA in the data.frame
# This is part of a project to create a package to read epx-files from EpiData into R

# Johann Popp
# 2019-06-09
# Last modification: 2023-11-14
###########################################


#' Helper: Convert defined missings to NA
#'
#' A helper function of \code{\link{read.EpiData}}, to convert defined missing values to NA.
#'
#' @param dat A data.frame or a list of data.frames created by \code{\link{epx.extract}}.
#' @param info Additional information about the EpiData-file created by \code{\link{epx.extract}}.
#'
#' @return A data.frame with defined missing values set to NA.
#' @export
#'
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

epx.missing <- function(dat, info){
  dat <- dat
  info <- info

  # Extract value codes
  valueCodes <- lapply(info$valLabels, function(x) xml2::xml_attr(x, "value"))
  # Identify value label sets that have codes for missing values and where are those codes
  indexMissing <- lapply(info$valLabels, function(x) grep("missing=\"true\"", x))
  # Identify missing value codes
  missingCodes <- as.list(mapply(function(x, y) x[y], valueCodes, indexMissing))

  # Index the corresponding value label set for each of the variables
  indexValLabSet <- lapply(info$fieldValLabSets, function(x) which(xml2::xml_attr(info$valLabelSets, "id") == x))
  # Missings per variable
  missingsPerVar <- lapply(missingCodes[as.numeric(paste(indexValLabSet))],
                           paste, collapse = "|")
  missingsPerVar[missingsPerVar == ""] <- NA

  # # Convert defined missing values into NA
  # dat[
  #   mapply(function(x, y){
  #     grepl(y, x)
  #   },
  #   x = dat,
  #   y = missingsPerVar) == 1
  #   ] <- NA

  # Identify defined missing values in the data.frame
  defMissings <-
    mapply(function(x, y){
      grepl(y, x)
    },
    x = dat,
    y = missingsPerVar)

  # Convert defined missing values to NA
  ifelse(nrow(dat) >1,
         dat[defMissings] <- NA,
         dat[,which(defMissings)] <- NA)

  # Convert undefined missing values in text fields (expressed as "NA") into NA
  dat[dat == "NA"] <- NA

  dat
}
