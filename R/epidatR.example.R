#' Path to epidatR Example files
#'
#' This is just a helper function to get the path to the example files of the attached epidatR package.
#'
#' @param x Example epx-file.
#'
#' @return Complete file path.
#' @export
#'
#' @examples
#' epidatR.example("marathon.epx")
#'
epidatR.example <- function(x){
  paste0(grep("epidatR", searchpaths(), value = TRUE), "/extdata/", x)
}
