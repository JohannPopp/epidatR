# Function to replace value codes with value labels extracted from an epx-file.


# This is part of a project to create a package to read epx-files from EpiData into R

# Johann Popp
# 2023-11-20
# Last modification 2023-11-22
###########################################


#' Helper: Replace value codes with value labels extracted from an epx-file
#'
#' A helper function of \code{\link{read.EpiData}}, to replace values by value labels and convert variables to appropriate classes.
#'
#' @param dat A data.frame or a list of data.frames created by \code{\link{epx.extract}}.
#' @param info Additional information about the EpiData-file created by \code{\link{epx.extract}}.
#'
#' @return A data.frame containing value labels instead of value codes.
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



epx.labels <- function(dat, info){


  # Extract field types
  fieldTypes <- info$fieldTypes
  # Extract value lable sets that contain codes for non-missing values
  catValLabelSets <- info$valLabelSets#[lapply(info$valLabels, function(x) sum(!grepl("missing=\"true\"", x))) > 0]
  # Extract value labels
  catValLabels <- lapply(catValLabelSets, xml2::xml_children)
  # value codes
  valCodes <- lapply(catValLabels, function(x) xml2::xml_attr(x, "value"))
  # Value labels
  validLabels <- lapply(catValLabels, xml2::xml_text)


  # Indicate which value set matches which variable
  indexValLabelSet  <- as.numeric(
    lapply(
      info$fieldValLabSets,
      function(x){
        which(
          grepl(
            paste0("^", x, "$"),
            xml2::xml_attr(catValLabelSets, "id")))
      }
    )
  )


  # Replace value codes with value labels
  dat2 <-
    mapply(
      function(x, y, z){
        if(!is.na(y) & (z == "ftInteger" | z == "ftFloat")){

          validLabels[[y]][as.numeric(lapply(x, function(x)  which(x == valCodes[[y]])))]
        } else {
          if(!is.na(y) & z == "ftString"){
            validLabels[[y]][factor(x, levels = valCodes[[y]])]
          } else {x}
        }
      },
      x = dat, y = indexValLabelSet, z = fieldTypes)
  dat2[is.na(dat2)] <- dat[is.na(dat2)]


  ifelse(nrow(dat) >1,
         dat <- as.data.frame(dat2, stringsAsFactors = FALSE),
         dat <- as.data.frame(t(dat2), stringsAsFactors = FALSE))




  # Reassign variable labels
  attributes(dat)$variable.labels <- info$fieldLabels



  dat
}


