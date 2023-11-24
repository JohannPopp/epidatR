#' Path to epidatR Example files
#'
#' This is just a helper function to get the path to the example files of the attached epidatR package.
#'
#' @param path Name of the example file. If NULL, all example files will be listed.
#'
#' @return Complete file path.
#' @export
#'
#' @examples
#' epx.example("SomeFakeData.epx")


epx.example <- function (path = NULL){
  if (is.null(path)) {
    dir(system.file("extdata", package = "epidatR"))
  }
  else {
    system.file("extdata", path, package = "epidatR", mustWork = TRUE)
  }

  # paste0(grep("epidatR", searchpaths(), value = TRUE), "/extdata/", x)
}




