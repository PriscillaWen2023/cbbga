#' Launches the Shiny app included in the package.
#'
#' This function launches the Shiny app that is included in the package.
#' Users can interact with the Shiny app by entering a team name to view
#' statistics and plots related to that team.
#'
#' @export
#' @examples
#' \dontrun{
#' launch_shiny_app()
#' }
launch_shiny_app <- function() {
  shiny::runApp(system.file("shiny", package = "cbbga"))
}