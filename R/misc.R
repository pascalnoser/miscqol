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

  # Range covering inserted block
  new_range <- ctx$selection[[1]]$range
  end_row <- new_range$end["row"]
  inserted_range <- rstudioapi::document_range(
    start = new_range$start,
    end   = rstudioapi::document_position(end_row + 2, 999)
  )

  # Reindent the block
  rstudioapi::setSelectionRanges(inserted_range, id = ctx$id)
  rstudioapi::executeCommand("reindent")

  # Re-fetch document after reindent
  ctx2 <- rstudioapi::getActiveDocumentContext()
  first_line <- ctx2$contents[new_range$start["row"]]

  # Find the "if (" and place cursor after it
  col_if <- regexpr("if \\(", first_line)
  if (col_if > 0) {
    cursor_position <- rstudioapi::document_position(
      row = new_range$start["row"],
      column = col_if + attr(col_if, "match.length")
    )
    rstudioapi::setCursorPosition(cursor_position, id = ctx$id)
  }
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

  # Build text
  new_text <- sprintf("for () {\n\t%s\n}", sel)

  # Replace selection
  rstudioapi::modifyRange(ctx$selection[[1]]$range, new_text, id = ctx$id)

  # Range covering inserted block (2 lines more than selection)
  new_range <- ctx$selection[[1]]$range
  end_row <- new_range$end["row"]
  inserted_range <- rstudioapi::document_range(
    start = new_range$start,
    end   = rstudioapi::document_position(end_row + 2, 999)
  )

  # Reindent the block
  rstudioapi::setSelectionRanges(inserted_range, id = ctx$id)
  rstudioapi::executeCommand("reindent")

  # Re-fetch document after reindent
  ctx2 <- rstudioapi::getActiveDocumentContext()
  first_line <- ctx2$contents[new_range$start["row"]]

  # Find the "for (" and place cursor after it
  col_for <- regexpr("for \\(", first_line)
  if (col_for > 0) {
    cursor_position <- rstudioapi::document_position(
      row = new_range$start["row"],
      column = col_for + attr(col_for, "match.length")
    )
    rstudioapi::setCursorPosition(cursor_position, id = ctx$id)
  }
}

#' Assign the loop variable to the first value in its sequence
#'
#' If the current line begins with a `for` loop (e.g. `for (var in expr)`), it
#' extracts the loop variable and the loop expression, then assigns the variable
#' to the first element of the expression in the current R session.
#'
#' #' @details
#' The function only looks at the single line where the cursor is positioned.
#' Multi-line loop headers are not supported. It recognizes variable names
#' consisting of letters, digits, underscores, or dots. Trailing comments (`#
#' ...`) and optional braces after the `for` statement are tolerated.
#'
#' Supported loop forms include (with or without trailing `{` or comments):
#' \itemize{
#'   \item `for (i in 1:10)`
#'   \item `for(i in seq_along(x)){`
#'   \item `for (element_name in names(ls_elements))`
#'   \item `for(value in myfun(x, y)) # with trailing comment`
#' }
#'
#' @export
assign_first_loop_value <- function() {
  ctx  <- rstudioapi::getActiveDocumentContext()
  row  <- ctx$selection[[1]]$range$start[1]
  line <- ctx$contents[[row]]

  # Robust pattern
  pat <- "^\\s*for\\s*\\(\\s*([[:alnum:]_.]+)\\s+in\\s+(.+)\\)\\s*\\{?\\s*(?:#.*)?$"

  m  <- regexec(pat, line, perl = TRUE)
  mm <- regmatches(line, m)[[1]]

  if (length(mm) >= 3) {
    var <- mm[2]              # loop variable
    seq <- trimws(mm[3])      # loop expression
    cmd <- sprintf("%s <- (%s)[1]", var, seq)
    rstudioapi::sendToConsole(cmd, execute = TRUE)
  } else {
    message("No for loop detected on this line.")
  }
}
