# Function to combine the single functions of the project and bring everthing together.

# Arguments:
# x: an epx-file.
# raw.data: If FALSE, defined missings are ste to NA and
## variables are convertet to R-classes according to their field type.
## If TRUE, the data.frame contains the original codes as character variables.

# Value
# If the epx-file contains only one data set, the output will be a data.frame.
## It it is a relational data base wit multiple data sets. The outcome will be a list of data frames.
# Study information, variable labels and information about data.frame-relations will be given as attributes.

# This is part of a project to create a package to read epx-files from EpiData into R.

# So far, the functions depend on the package xml2


# Johann Popp
# 2019-06-16
# Last update: 2021-08-30
###########################################

#' Read EpiData epx-Files
#'
#' Reads \href{https://www.epidata.dk/}{EpiData} epx-files into a
#' \code{\link{data.frame}} or a \code{\link{list}} of \code{data.frame}s.
#'
#' @param x An epx-file created by EpiData.
#' @param convert logical. Shall variables be converted to appropriate object
#' classes? Shall the value labels of coded variables be shown?
#' @param setNA logical. Shall defined missings be set to \code{NA}?
#'
#' @return
#' A simple data base will be returned as a \code{data.frame}. A relational
#' data base will be returned as a \code{list} of \code{data.frame}s.
#'
#' Study Informations are stored as \code{attributes(...)$study.info} and variable
#' labels are stored as \code{attributes(...)$variable.labels}. For relational
#' data bases, key variables and parent data sets are stored as \code{attributes(...)$info.relations}.
#'
#' @details
#' By default, object classes of the variables will be set according to the field type defined in EpiData. Coded variables will be returned as \code{factor}s with the value labels defined in EpiData. Values that are defined as missing values in EpiData will be set to \code{NA}.
#'
#'  If this is not wanted or causes some trouble, you can set \code{convert=FALSE} to get \code{data.frame}s with the raw data in \code{character} form. Set \code{setNA=FALSE} to use the original codes/labels for defined missing values.
#'
#'
#' @export
#'
#' @examples
#' # Read the example data set "marathon.epx"
#' # (The helper function epidatR.example is only needed to identify the path
#' # of the example file in the loaded package).
#' path <- epidatR.example("marathon.epx")
#' read.EpiData(path)
#'
#' # Extract study information
#' attributes(path)$study.info



read.EpiData <- function(x, convert = TRUE, setNA = TRUE){
  info <- epx.extract(x)
  dat <- lapply(info[[7]], epx.read)

  # Combine dat and info in a data-set-wise list
  perDataSet <- mapply(function(dat, info) list(list(dat = dat, info = info)), dat, info[[7]])

  suppressWarnings(
    if(setNA == TRUE){
      dat2 <- lapply(perDataSet, function(x) epx.missing(x$dat, x$info))
      perDataSet <- mapply(function(dat, info) list(list(dat = dat, info = info)), dat2, info[[7]])
    })

  suppressWarnings(
    if(convert == TRUE){
      dat2 <- lapply(perDataSet,
                     function(x){
                       if(inherits(x[[1]], "data.frame")){
                         epx.class(x[[1]], x[[2]])
                       } else {"This data frame has no entries"}
                     })
      attributes(dat2) <- attributes(dat)
      dat2
    } else {
      dat2 <- dat
    }
  )


  if(length(dat2) == 1){
    dat2 <- dat2[[1]]
  }

  # Extract study information
  infoStudy <- lapply(xml2::xml_children(info$infoStudy), xml2::xml_text)
  names(infoStudy) <- xml2::xml_name(xml2::xml_children(info$infoStudy))
  attributes(dat2)$study.info <- t(data.frame(infoStudy))

  # Collect relational information
  if(length(dat) > 1){
    attributes(dat2)$info.relations <-
      data.frame(data.set = xml2::xml_attr(info$infoDataSets, "id"),
                 parent.data.set = unlist(info$infoParentDataSet),
                 key.variables = unlist(info$infoKeyVars))
  }

  dat2
}




##############
# The next step will be, to enable the reading of encrypted files
