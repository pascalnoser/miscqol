#' Report memory usage of objects in an environment
#'
#' Computes the memory usage of all objects in a given environment, returning a
#' named vector sorted from largest to smallest. Sizes can be reported in fixed
#' units or automatically scaled per object for improved readability.
#'
#' @param env An environment from which to report object sizes. Defaults to
#'   \code{.GlobalEnv}.
#' @param units Character string specifying the units for reporting sizes. One
#'   of \code{"auto"}, \code{"bytes"}, \code{"KB"}, \code{"MB"}, or \code{"GB"}.
#'   When \code{"auto"}, each object is reported using the largest appropriate
#'   unit.
#' @param numeric If \code{TRUE}, return a numeric vector instead of formatted
#'   character output. When \code{units = "auto"}, numeric output is returned in
#'   bytes.
#'
#' @return A named vector sorted in decreasing order of memory usage.
#' \describe{
#'   \item{If \code{numeric = FALSE}}{A character vector with formatted
#'   sizes and unit labels.}
#'   \item{If \code{numeric = TRUE}}{A numeric vector of sizes in the
#'   requested units (or bytes when \code{units = "auto"}).}
#' }
#'
#' @details
#' Memory usage is estimated using \code{object.size()} and may overestimate
#' actual memory consumption due to shared memory and copy-on-write behavior.
#'
#' Objects are sorted by their raw size in bytes, regardless of the reporting
#' units.
#'
#' @examples
#' x <- rnorm(1e6)
#' y <- matrix(1, 1e4, 1e4)
#'
#' # Automatically choose best unit per object
#' env_object_sizes()
#'
#' # Fixed units
#' env_object_sizes(units = "MB")
#'
#' # Numeric output
#' env_object_sizes(units = "GB", numeric = TRUE)
#'
#' @seealso \code{\link{object.size}}, \code{\link{ls}}
#'
#' @export
env_object_sizes <- function(
    env = .GlobalEnv,
    units = c("auto", "bytes", "KB", "MB", "GB"),
    numeric = FALSE
) {
  units <- match.arg(units)

  objs <- ls(envir = env)

  sizes_bytes <- vapply(
    objs,
    function(x) as.numeric(object.size(get(x, envir = env))),
    numeric(1)
  )

  names(sizes_bytes) <- objs

  # sort largest to smallest (by bytes)
  ord <- order(sizes_bytes, decreasing = TRUE)
  sizes_bytes <- sizes_bytes[ord]

  if (units == "auto") {

    if (numeric) {
      # cannot return consistent units; return bytes
      return(sizes_bytes)
    }

    format_auto <- function(x) {
      if (x >= 1024^3) {
        paste0(formatC(x / 1024^3, digits = 3, format = "f"), " GB")
      } else if (x >= 1024^2) {
        paste0(formatC(x / 1024^2, digits = 3, format = "f"), " MB")
      } else if (x >= 1024) {
        paste0(formatC(x / 1024, digits = 3, format = "f"), " KB")
      } else {
        paste0(x, " bytes")
      }
    }

    return(setNames(vapply(sizes_bytes, format_auto, character(1)),
                    names(sizes_bytes)))
  }

  # fixed unit conversion
  div <- switch(
    units,
    bytes = 1,
    KB = 1024,
    MB = 1024^2,
    GB = 1024^3
  )

  sizes <- sizes_bytes / div

  if (numeric) {
    return(sizes)
  } else {
    return(setNames(
      paste0(formatC(sizes, format = "f", digits = 3), " ", units),
      names(sizes)
    ))
  }
}
