#' Function launching the app..;
#' @return None
#' @examples
#' if (interactive()) PDX_app()
#' @export
Launch_myApp <- function() {
shinyApp(ui = ui_myApp, server = server_myApp)
}
