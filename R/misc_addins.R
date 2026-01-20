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
#' Wraps the current line in an `if (...) { <text> }` block and places the
#' cursor inside the `if ()` parentheses. If text is selected, wraps all lines
#' that are at least partially included in the selection.
#'
#' @export
wrap_in_if <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  if (length(ctx$selection) == 0) return(invisible())

  sel <- ctx$selection[[1]]
  sel_range <- sel$range

  # Expand range to whole lines if selection is empty or partial
  if (identical(sel$text, "")) {
    # No selection: wrap current line
    start_row <- sel_range$start["row"]
    end_row <- start_row
  } else {
    # Expand to full lines covering the selection
    start_row <- sel_range$start["row"]
    end_row <- sel_range$end["row"]
  }

  # Grab all lines to be wrapped
  lines <- ctx$contents[start_row:end_row]
  text_block <- paste(lines, collapse = "\n")

  # Build wrapped text
  new_text <- sprintf("if () {\n\t%s\n}", text_block)

  # Replace full line range
  line_range <- rstudioapi::document_range(
    start = rstudioapi::document_position(start_row, 1),
    end   = rstudioapi::document_position(end_row, nchar(lines[length(lines)]) + 1)
  )
  rstudioapi::modifyRange(line_range, new_text, id = ctx$id)

  # Adjust end row after adding if block
  line_range$end[1] <- end_row + 2

  # Reindent the block
  rstudioapi::setSelectionRanges(line_range, id = ctx$id)
  rstudioapi::executeCommand("reindent")

  # Re-fetch document after reindent
  ctx2 <- rstudioapi::getActiveDocumentContext()
  first_line <- ctx2$contents[start_row]

  # Place cursor inside the if parentheses
  col_if <- regexpr("if \\(", first_line)
  if (col_if > 0) {
    cursor_position <- rstudioapi::document_position(
      row = start_row,
      column = col_if + attr(col_if, "match.length")
    )
    rstudioapi::setCursorPosition(cursor_position, id = ctx$id)
  }
}

#' Wrap selection in a for loop
#'
#' Wraps the current line in a `for (...) { <text> }` block and places the
#' cursor inside the `for ()` parentheses. If text is selected, wraps all lines
#' that are at least partially included in the selection.
#'
#' @export
wrap_in_for <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  if (length(ctx$selection) == 0) return(invisible())

  sel <- ctx$selection[[1]]
  sel_range <- sel$range

  # Expand range to whole lines if selection is empty or partial
  if (identical(sel$text, "")) {
    # No selection: wrap current line
    start_row <- sel_range$start["row"]
    end_row   <- start_row
  } else {
    # Expand to full lines covering the selection
    start_row <- sel_range$start["row"]
    end_row   <- sel_range$end["row"]
  }

  # Grab all lines to be wrapped
  lines <- ctx$contents[start_row:end_row]
  text_block <- paste(lines, collapse = "\n")

  # Build wrapped text
  new_text <- sprintf("for () {\n%s\n}", text_block)

  # Replace full line range
  line_range <- rstudioapi::document_range(
    start = rstudioapi::document_position(start_row, 1),
    end   = rstudioapi::document_position(end_row, nchar(lines[length(lines)]) + 1)
  )
  rstudioapi::modifyRange(line_range, new_text, id = ctx$id)

  # Adjust end row after adding if block
  line_range$end[1] <- end_row + 2

  # Reindent the block
  rstudioapi::setSelectionRanges(line_range, id = ctx$id)
  rstudioapi::executeCommand("reindent")

  # Re-fetch document after reindent
  ctx2 <- rstudioapi::getActiveDocumentContext()
  first_line <- ctx2$contents[start_row]

  # Place cursor inside the for parentheses
  col_for <- regexpr("for \\(", first_line)
  if (col_for > 0) {
    cursor_position <- rstudioapi::document_position(
      row = start_row,
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
#' #' @details The function only looks at the single line where the cursor is
#' positioned. Multi-line loop headers are not supported. It recognizes variable
#' names consisting of letters, digits, underscores, or dots. Trailing comments
#' (`# ...`) and optional braces after the `for` statement are tolerated.
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
  col  <- ctx$selection[[1]]$range$start[2]
  line <- ctx$contents[[row]]

  # Robust pattern
  pat <- "^\\s*for\\s*\\(\\s*([[:alnum:]_.]+)\\s+in\\s+(.+)\\)\\s*\\{?\\s*(?:#.*)?$"
  m  <- regexec(pat, line, perl = TRUE)
  mm <- regmatches(line, m)[[1]]

  if (length(mm) >= 3) {
    var <- mm[2]  # loop variable
    seq <- trimws(mm[3])  # loop expression
    cmd <- sprintf("%s <- (%s)[1]", var, seq)

    # Execute in console to explicitly see assignment
    rstudioapi::sendToConsole(cmd, execute = TRUE)

    # Move focus back to source and restore cursor location
    rstudioapi::executeCommand("activateSource")
    rstudioapi::setCursorPosition(rstudioapi::document_position(row, col), id = ctx$id)
  } else {
    message("No for loop detected on this line.")
  }
}
