#' Run Shiny App with preset USFSA Events to choose from
#'
#' @return shiny application
#' @export
#'
#' @importFrom shiny runApp
#' @examples
runEventApp <- function() {
  appDir <- system.file("myapp", package = "USFSAevents")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `USFSAevents`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
