#' Calculate max decimals
#'
#' @param x A numeric vector
#'
#' @returns A numeriv value, the maximum number of decimal points
#' @export
#'
max_decimals <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]

  if (!is.numeric(x)) stop("Input must be numeric")
  if (length(x) == 0) return(NA_integer_)

  decimals <- sapply(x, function(val) {
    if (val %% 1 == 0) return(0)
    dec_str <- sub("^[^.]*\\.", "", sub("0+$", "", as.character(val)))
    nchar(dec_str)
  })

  max(decimals)
}
