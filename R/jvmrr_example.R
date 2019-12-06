##' Get path to example file
##'
##' @param name file name
##' @export
jvmrr_example <- function(name = NULL) {
  if (is.null(name)) {
    dir(system.file("java", package = "jvmrr"))
  } else {
    system.file("java", name, package = "jvmrr", mustWork = TRUE)
  }
}
