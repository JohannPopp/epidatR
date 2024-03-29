% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractInformation.R
\name{epx.extract}
\alias{epx.extract}
\title{Helper: Extract Data From Epx-File}
\usage{
epx.extract(x)
}
\arguments{
\item{x}{An EpiData epx-file}
}
\value{
A list, containing the following elements:
    \describe{
      \item{$epx:}{The entire xml_document from the epx-file.}
      \item{$infoEpiData:}{Information about EpiDatas set up.}
      \item{$infoStudy}{Information about the study.}
      \item{$infoSeparators}{Used separators.}
      \item{$infoDataSets}{Summary information about the data sets.}
      \item{$infoParentDataSet}{Information about parent data sets in relational data bases.}
      \item{$perDataSet}{Information from each data set.}
      \item{$infoKeyVars}{Key variables in relational data bases.}
   }
}
\description{
A helper function of \code{\link{read.EpiData}}, to extract the xml-like data from an epx-file.
}
\examples{
# This demonstrates the effects of all six helper functions to \code{\link{read.EpiData}}.

(x <- epx.example("SomeFakeData.epx"))

# epx.extract() extracts the information from an EpiData-epx file
(info <- epx.extract(x))

# epx.read() transforms the information into a data.frame
(dat <- lapply(info$perDataSet, epx.read))
# Combine dat and info in a data-set-wise list
# This is a necessary detour to handle EpiData files with multiple data sets.
perDataSet <- mapply(function(dat, info) list(list(dat = dat, info = info)), dat, info[[7]])

# epx.labels() replaces value codes with value labels
(datLab <- epx.labels(perDataSet$ds1$dat, perDataSet$ds1$info))

# epx.missing() replaces definde missing values with NA
(datMis <- epx.missing(perDataSet$ds1$dat, perDataSet$ds1$info))

# epx.class() sets variable classes according to the field types defined in EpiData
(datClass <- epx.class(perDataSet$ds1$dat, perDataSet$ds$info))

# Bringing it all together
read.EpiData(epx.example("SomeFakeData.epx"))
}
