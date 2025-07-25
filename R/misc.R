#' Navigate to directory of open file
#'
#' Open the Files pane and navigate to the folder containing the file currently
#' open in the editor.
#'
#' @export
#'
navigate_to_active_file <- function() {
  open_file <- rstudioapi::getSourceEditorContext()$path
  rstudioapi::filesPaneNavigate(open_file)
  rstudioapi::executeCommand("activateFiles")
}

#' Wrap in parentheses
#'
#' Wrap the selected text in parentheses and move the cursor to the front
#'
#' @export
wrap_in_parens <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  sel <- ctx$selection[[1]]$text

  # Wrap the selected text in parentheses
  new_text <- paste0("(", sel, ")")

  # Replace selection
  rstudioapi::modifyRange(ctx$selection[[1]]$range, new_text)

  # Move cursor to the start (before the parentheses)
  new_range <- ctx$selection[[1]]$range
  new_range$end[[2]] <- new_range$start[[2]] + 1  # Move cursor just after "("
  rstudioapi::setCursorPosition(new_range$start)  # Cursor at start
}
