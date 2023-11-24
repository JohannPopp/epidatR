# Function to extract xml-type information from a epx-file that was produced with EpiData 4.6.
# This expands the function to the case of multiple data sets in one file
# This is part of a project to create a package to read epx-files into R

# Johann Popp
# 2019-06-08
# Last update: 2023-11-23
###########################################


#' Helper: Extract Data From Epx-File
#'
#' A helper function of \code{\link{read.EpiData}}, to extract the xml-like data from an epx-file.
#'
#' @param x An EpiData epx-file
#'
#' @return
#'     A list, containing the following elements:
#'     \describe{
#'       \item{$epx:}{The entire xml_document from the epx-file.}
#'       \item{$infoEpiData:}{Information about EpiDatas set up.}
#'       \item{$infoStudy}{Information about the study.}
#'       \item{$infoSeparators}{Used separators.}
#'       \item{$infoDataSets}{Summary information about the data sets.}
#'       \item{$infoParentDataSet}{Information about parent data sets in relational data bases.}
#'       \item{$perDataSet}{Information from each data set.}
#'       \item{$infoKeyVars}{Key variables in relational data bases.}
#'    }

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

epx.extract <- function(x){
  # Read epx-data and remove name spaces.
  epx <- xml2::xml_ns_strip(xml2::read_xml(x))

  # Extract general Informations
  infoEpiData <- xml2::xml_attrs(xml2::xml_find_all(epx, "//EpiData"))
  infoStudy <- xml2::xml_find_all(epx, "//StudyInfo")
  infoSeparators <- unlist(xml2::xml_attrs(xml2::xml_find_all(epx, "//Settings")))

  infoDataSets <- xml2::xml_find_all(epx, "//DataFile")

  # Data set relations
  level <- xml2::xml_find_all(epx, "//DataFileRelations") |>
    xml2::xml_children()
  infoRelations <- data.frame(data.set = xml2::xml_attr(level, "dataFileRef"))
  level <- lapply(level, xml2::xml_find_all, ".//DataFileRelation")
  child <- lapply(level, xml2::xml_attr, "dataFileRef")
  child <- unlist(lapply(child, paste, collapse = "; "))
  infoRelations$offspring.data.sets = child

  # Key variables
  infoKeyVars <- data.frame(
    data.set =
      xml2::xml_find_all(epx, "//DataFile") |>
      xml2::xml_attr("id"),
    labels =
      xml2::xml_find_all(epx, "//DataFile") |>
      xml2::xml_child("Caption") |>
      xml2::as_list() |> unlist(),
    key =
      xml2::xml_find_all(epx, "//DataFile") |>
      lapply(xml2::xml_child, "KeyFields") |>
      lapply(xml2::xml_children) |>
      lapply(xml2::xml_attr, "fieldRef") |>
      lapply(paste, collapse = "; ") |>
      as.character()
  )

  # Merge with infoRelations
  infoRelations <-
    merge(infoRelations, infoKeyVars, by = "data.set")[,c(1,3,2,4)]

  # Extract information for each data set
  epxExtractDataSet <- function(df){

    # Extract data base entries
    records <-
      xml2::xml_text(
        xml2::xml_find_all(
          xml2::xml_find_all(df, "Records"),
          "Record")
      )

    # Extract field definitions
    datFields <- xml2::xml_find_all(df, ".//Field")
    # Field names
    fieldNames <- xml2::xml_attr(datFields, "id")
    # Field labels
    fieldLabels <- xml2::xml_text(datFields)
    # Field types
    fieldTypes <- xml2::xml_attr(datFields, "type")

    # Value label sets
    # Indicate which fields have which value label sets
    fieldValLabSets <- xml2::xml_attr(datFields, "valueLabelRef")
    # Extract value labels (all value label sets for all data sets)
    valLabelSets <- xml2::xml_find_all(epx, "//ValueLabelSet")
    valLabels <- lapply(valLabelSets, function(x) xml2::xml_find_all(x, ".//ValueLabel"))

    # Extract separators
    infoSeparators <- infoSeparators

    # Key variables
    # keyVars <- xml2::xml_attr(xml2::xml_find_all(df,  ".//Key"), "fieldRef")


    # Gather information in a list
    list(records = records, datFields = datFields, fieldNames = fieldNames, fieldLabels = fieldLabels,
         fieldTypes = fieldTypes, fieldValLabSets = fieldValLabSets, valLabelSets = valLabelSets,
         valLabels = valLabels, infoSeparators = infoSeparators)

  }

  perDataSet <- lapply(infoDataSets, epxExtractDataSet)
  names(perDataSet) <- xml2::xml_attr(infoDataSets, "id")





  # Gather information in a list
  list(epx = epx, infoEpiData = infoEpiData, infoStudy = infoStudy, infoSeparators = infoSeparators,
       infoDataSets = infoDataSets, infoRelations = infoRelations,
       perDataSet = perDataSet)
}



