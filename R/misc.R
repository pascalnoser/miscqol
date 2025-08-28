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

#' Wrap selection in parentheses
#'
#' Wraps the currently selected text in parentheses and moves the cursor to the
#' front.
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

#' Wrap selection in an if statement
#'
#' Wraps the currently selected text in an if(...) { ... } block.
#'
#' @export
wrap_in_if <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  if (length(ctx$selection) == 0) return(invisible())

  sel <- ctx$selection[[1]]$text
  if (identical(sel, "")) return(invisible())

  # Build text
  new_text <- sprintf("if () {\n\t%s\n}", sel)

  # Replace selection
  rstudioapi::modifyRange(ctx$selection[[1]]$range, new_text, id = ctx$id)

  # Determine new cursor position
  new_range <- ctx$selection[[1]]$range
  start_row <- new_range$start["row"]
  start_col <- new_range$start["column"]

  # Cursor should land right after "if ("
  cursor_position <- rstudioapi::document_position(
    row = start_row,
    column = start_col + 4  # "if (" is 4 chars
  )

  rstudioapi::setCursorPosition(cursor_position, id = ctx$id)
}

#' Wrap selection in a for statement
#'
#' Wraps the currently selected text in a for (...) { ... } block.
#'
#' @export
wrap_in_for <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  if (length(ctx$selection) == 0) return(invisible())

  sel <- ctx$selection[[1]]$text
  if (identical(sel, "")) return(invisible())

  # Build text with placeholder "condition"
  new_text <- sprintf("for () {\n\t%s\n}", sel)

  # Replace selection
  rstudioapi::modifyRange(ctx$selection[[1]]$range, new_text, id = ctx$id)

  # Determine new cursor position
  new_range <- ctx$selection[[1]]$range
  start_row <- new_range$start["row"]
  start_col <- new_range$start["column"]

  # Cursor should land right after "for ("
  cursor_position <- rstudioapi::document_position(
    row = start_row,
    column = start_col + 5  # "for (" is 5 chars
  )

  rstudioapi::setCursorPosition(cursor_position, id = ctx$id)
}
