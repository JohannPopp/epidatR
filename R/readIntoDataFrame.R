# Function to convert exracted information from an epx-file into a data.frame
# This is part of a project to create a package to read epx-files from EpiData into R

# Johann Popp
# 2019-06-23
# Last update: 2019-08-02
###########################################


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

