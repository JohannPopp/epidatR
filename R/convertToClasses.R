# Function to convert into R-object classes according to the field types of the epx-data.

## So far the function can read and convert epx.Projects with one data set and without encryption.

# This is part of a project to create a package to read epx-files from EpiData into R

# Johann Popp
# 2019-06-11
# Last modification 2023-11-20
###########################################


#' Helper: Show value labels and convert variables to appropriate classes
#'
#' A helper function of \code{\link{read.EpiData}}, to replace values by value labels and convert variables to appropriate classes.
#'
#' @param dat A data.frame or a list of data.frames created by \code{\link{epx.extract}}.
#' @param info Additional information about the EpiData-file created by \code{\link{epx.extract}}.
#'
#' @return A data.frame with value labels and appropriate object classes of the variables.
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


epx.class <- function(dat, info){

  # Extract field types
  fieldTypes <- info$fieldTypes
  # # Extract value lable sets that contain codes for non-missing values
  # catValLabelSets <- info$valLabelSets#[lapply(info$valLabels, function(x) sum(!grepl("missing=\"true\"", x))) > 0]
  # Extract value labels
  catValLabels <- lapply(info$valLabelSets, xml2::xml_children)
  # # value codes
  # valCodes <- lapply(catValLabels, function(x) xml2::xml_attr(x, "value"))
  # # Value labels
  # validLabels <- lapply(validCategories, xml2::xml_text)
  # Names of value label sets
  valLabSets <- xml2::xml_attr(info$valLabelSets, "id")



  # Indicate which value set matches which variable
  indexValLabelSet  <- as.numeric(
    lapply(
      info$fieldValLabSets,
      function(x){
        which(
          grepl(
            paste0("^", x, "$"),
            xml2::xml_attr(info$valLabelSets, "id")))
      }
    )
  )

  # Which label sets contain only labels for missing values?
  missOnlySet <-
    valLabSets[
      unlist(
        lapply(catValLabels, function(x){
          all(grepl("missing=\"true\"", x))
        })
      )]


    # Convert integer fields
    toInt <- (grepl(paste0("^", missOnlySet, "$", collapse = "|"), info$fieldValLabSets) |
                is.na(info$fieldValLabSets)) &
      grepl("ftInteger|ftAutoInc", fieldTypes)

    dat[,toInt] <-
      as.integer(
        unlist(
          dat[,toInt])
      )

    # Convert numeric fields
    toNum <- (grepl(paste0("^", missOnlySet, "$", collapse = "|"), info$fieldValLabSets) |
                is.na(info$fieldValLabSets)) &
      grepl("ftFloat", fieldTypes)

    if(sum(toNum) > 1){
      dat[, toNum] <-
        apply(dat[, toNum], 2, function(x) sub(info$infoSeparators[3], ".", x))
      dat[, toNum] <-
        apply(dat[, toNum], 2, as.numeric)
    } else {
      if(sum(toNum) == 1){
        dat[, toNum] <-
          as.numeric(
            sub(
              info$infoSeparators[3],
              ".",
              dat[, toNum]
            ))
      }
    }


    # Convert categorical fields
    notMissOnlySet <-
      valLabSets[!grepl(paste0("^", missOnlySet, "$", collapse = "|"), valLabSets)]

    toFac <- grepl(paste0("^", notMissOnlySet, "$", collapse = "|"), info$fieldValLabSets) |
      fieldTypes == "ftString"

    if(sum(toFac) == 1){
      dat[, toFac] <-
        factor(dat[, toFac])
    } else {
      if(sum(toFac) > 1){
        # dat[, !is.na(indexValLabelSet)] <-
        #   apply(dat[, !is.na(indexValLabelSet)], 2, factor)
        dat[, toFac] <-
          as.data.frame(lapply(dat[, toFac], factor))
      }
    }


    # Convert date fields
    ## DMY format
    dmyPattern <- paste("%d", info$infoSeparators[[1]][1], "%m", info$infoSeparators[[1]][1], "%Y", sep = "")
    if(sum(fieldTypes == "ftDMYDate" | fieldTypes == "ftDMYAuto") > 1){
      dat[, fieldTypes == "ftDMYDate" | fieldTypes == "ftDMYAuto"] <-
        lapply(dat[,fieldTypes == "ftDMYDate" | fieldTypes == "ftDMYAuto"], function(x) strptime(x, dmyPattern))
    } else {
      dat[,fieldTypes == "ftDMYDate" | fieldTypes == "ftDMYAuto"] <-
        data.frame(strptime(dat[,fieldTypes == "ftDMYDate" | fieldTypes == "ftDMYAuto"], dmyPattern))
    }

    ## MDY format
    mdyPattern <- paste("%m", info$infoSeparators[[1]][1], "%d", info$infoSeparators[[1]][1], "%Y", sep = "")
    if(sum(fieldTypes == "ftMDYDate" | fieldTypes == "ftMDYAuto") > 1){
      dat[, fieldTypes == "ftMDYDate" | fieldTypes == "ftMDYAuto"] <-
        lapply(dat[,fieldTypes == "ftMDYDate" | fieldTypes == "ftMDYAuto"], function(x) strptime(x, mdyPattern))
    } else {
      dat[,fieldTypes == "ftMDYDate" | fieldTypes == "ftMDYAuto"] <-
        data.frame(strptime(dat[,fieldTypes == "ftMDYDate" | fieldTypes == "ftMDYAuto"], mdyPattern))
    }

    ## YMD format
    ymdPattern <- paste("%Y", info$infoSeparators[[1]][1], "%m", info$infoSeparators[[1]][1], "%d", sep = "")
    if(sum(fieldTypes == "ftYMDDate" | fieldTypes == "ftYMDAuto") > 1){
      dat[, fieldTypes == "ftYMDDate" | fieldTypes == "ftYMDAuto"] <-
        lapply(dat[,fieldTypes == "ftYMDDate" | fieldTypes == "ftYMDAuto"], function(x) strptime(x, ymdPattern))
    } else {
      dat[,fieldTypes == "ftYMDDate" | fieldTypes == "ftYMDAuto"] <-
        data.frame(strptime(dat[,fieldTypes == "ftYMDDate" | fieldTypes == "ftYMDAuto"], ymdPattern))
    }

    # Convert time
    timePattern <- paste("%H", info$infoSeparators[2], "%M", info$infoSeparators[2], "%S", sep = "")
    if(sum(fieldTypes == "ftTime" | fieldTypes == "ftTimeAuto") > 1){
      dat[, fieldTypes == "ftTime" | fieldTypes == "ftTimeAuto"] <-
        lapply(dat[,fieldTypes == "ftTime" | fieldTypes == "ftTimeAuto"], function(x) strptime(x, timePattern))
    } else {
      dat[,fieldTypes == "ftTime" | fieldTypes == "ftTimeAuto"] <-
        data.frame(strptime(dat[,fieldTypes == "ftTime" | fieldTypes == "ftTimeAuto"], timePattern))
    }

    # Convert to logical fields
    dat[, fieldTypes == "ftBoolean"] <-
      dat[, fieldTypes == "ftBoolean"] == "Y"


  # Reassign variable labels
  attributes(dat)$variable.labels <- info$fieldLabels



  dat
}


