#' In Files pane, navigate to folder of file currently open in editor
#'
#' @export
#'
navigate_to_active_file <- function() {
  open_file <- rstudioapi::getSourceEditorContext()$path
  rstudioapi::filesPaneNavigate(open_file)
}
