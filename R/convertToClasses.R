# Function to convert into R-object classes according to the field types of the epx-data.

## So far the function can read and convert epx.Projects with one data set and without encryption.

# This is part of a project to create a package to read epx-files from EpiData into R

# Johann Popp
# 2019-06-11
# Last modification 2019-07-25
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
#'
epx.class <- function(dat, info){
  # dat <- x
  # info <- info

  # Extract field types
  fieldTypes <- info$fieldTypes
  # Extract value lable sets that contain codes for non-missing values
  catValLabelSets <- info$valLabelSets#[lapply(info$valLabels, function(x) sum(!grepl("missing=\"true\"", x))) > 0]
  # Extract value labels
  catValLabels <- lapply(catValLabelSets, xml2::xml_children)
  # Extract value labels that do not code missing values
  validCategories <- lapply(catValLabels, function(x) x)#[grepl("missing=\"true\"", x) == FALSE])
  # value codes
  validValCodes <- lapply(validCategories, function(x) xml2::xml_attr(x, "value"))
  # Value labels
  validLabels <- lapply(validCategories, xml2::xml_text)


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

          validLabels[[y]][as.numeric(lapply(x, function(x)  which(x == validValCodes[[y]])))]
        } else {
          if(!is.na(y) & z == "ftString"){
            validLabels[[y]][factor(x, levels = validValCodes[[y]])]
          } else {x}
        }
      },
      x = dat, y = indexValLabelSet, z = fieldTypes)#,

  ifelse(nrow(dat) >1,
         dat <- as.data.frame(dat2, stringsAsFactors = FALSE),
         dat <- as.data.frame(t(dat2), stringsAsFactors = FALSE))




  # Convert integer fields
  dat[,fieldTypes == "ftAutoInc" | fieldTypes == "ftInteger" & is.na(indexValLabelSet)] <-
    as.integer(
      unlist(
        dat[,fieldTypes == "ftAutoInc" | fieldTypes == "ftInteger" & is.na(indexValLabelSet)])
    )

  # Convert numeric fields
  if(sum(fieldTypes == "ftFloat" & is.na(indexValLabelSet)) > 1){
    dat[, fieldTypes == "ftFloat" & is.na(indexValLabelSet)] <-
      apply(dat[, fieldTypes == "ftFloat" & is.na(indexValLabelSet)], 2, function(x) sub(info$infoSeparators[3], ".", x))#sub(info$infoSeparators[[1]][3], ".", x))
    dat[, fieldTypes == "ftFloat" & is.na(indexValLabelSet)] <-
      apply(dat[, fieldTypes == "ftFloat" & is.na(indexValLabelSet)], 2, as.numeric)
  } else {
    if(sum(fieldTypes == "ftFloat" & is.na(indexValLabelSet)) == 1){
      dat[, fieldTypes == "ftFloat" & is.na(indexValLabelSet)] <-
        as.numeric(
          sub(
            info$infoSeparators[3],
            ".",
            dat[, fieldTypes == "ftFloat" & is.na(indexValLabelSet)]
          )
        )
    }
  }


  # Convert categorical fields
  if(sum(!is.na(indexValLabelSet)) == 1){
    dat[, !is.na(indexValLabelSet)] <-
      factor(dat[, !is.na(indexValLabelSet)])
  } else {
    if(sum(!is.na(indexValLabelSet)) > 1){
      # dat[, !is.na(indexValLabelSet)] <-
      #   apply(dat[, !is.na(indexValLabelSet)], 2, factor)
      dat[, !is.na(indexValLabelSet)] <-
        as.data.frame(lapply(dat[, !is.na(indexValLabelSet)], factor))
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


