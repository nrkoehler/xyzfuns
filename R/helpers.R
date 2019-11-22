#' @title Add UND or AND to Listing
#' @description Add UND or AND before the last element of a character vector.
#' @examples
#' \dontrun{add_and(LETTERS[1:3])}
#' @export
add_and <- function(words, sep = ", ", and = "und") {
  and <- paste0(" ", and, " ")
  n <- length(words)
  if (n == 0) {
    stop("Vector should contain at least one object!")
  }
  if (n == 1) {
    return(words)
  }
  if (n == 2) {
    return(paste(words, collapse = and))
  }
  if (grepl("^ ", and) && grepl(" $", sep)) {
    and <- gsub("^ ", "", and)
  }
  words[n] <- paste0(and, words[n])
  words <- paste(words, collapse = sep)
  words <- gsub(", und ", " und ", words)
  words
}
NULL
#' @title Compare Two Vectors
#' @description Compare two vectors and return intersections.
#' @source \url{https://twitter.com/tyluRp/status/1197634755430367235}
#' @examples
#' \dontrun{
#' x <- 1:4
#' y <- 3:6
#' compare(x, y)}
#' @export
compare_2_vecs <- function(x, y) {
  list(
    "Name of vector X" = deparse(substitute(x)),
    "Name of vector Y" = deparse(substitute(y)),
    "These values are in X not Y" = setdiff(x, y),
    "These values are in Y not X" = setdiff(y, x),
    "These values are shared between X and Y" = intersect(x, y),
    "Combined, X and Y returns these values" = union(x, y)
  )
}
NULL
#' @title Fix Variable Names with Encoding Errors
#' @description Fix variable names with encoding errors.
#' @examples
#' \dontrun{df <- df %>% fix_names}
#' @export
fix_names <- function(x) {
  names(x) <- enc2native(names(x))
  x
}
NULL
#' Format numbers.
#' @description
#' Format numbers (in R Markdown documents)
#' @param x  number
#' @param digits number of digits
#' @examples
#' \dontrun{
#' format_num(10.2589, digits = 2)
#' }
#' @export
format_num <- function(x, digits = 1) {
  formatC(round(x, digits = digits), format = "f", digits = digits)
}
NULL
#' Return 'more' vs. 'less' if value is positive vs. negative.
#' @description
#' Return 'more' vs. 'less' if value is positive vs. negative (in R Markdown documents)
#' @param x  number
#' @param rev  FALSE (default) for increasing trend, TRUE for decreasing trend (reduction).
#' @examples
#' \dontrun{
#' increase_more_less(0.2, ref = FALSE)
#' }
#' @export
more_less <- function(x, rev = FALSE) {
  if (rev == FALSE) {
    ifelse(x < 0, "less", "more")
  } else {
    ifelse(x < 0, "more", "less")
  }
}
NULL
#' @title Add Prefix and Leading Zero(s) to Variables
#' @description Add prefix and leading zero(s) to variables in order to get strings of equal length.
#' @examples
#' \dontrun{id <- c('1', '10', '100', '1000', '10000'); id_repair(id = a, prefix = 'CN_')}
#' @export
pad_var <- function(x, prefix = NULL) {
  x <- as.character(x)
  prefix <- as.character(prefix)
  z <- max(nchar(x))
  x <- stringr::str_pad(x, width = z, side = "left", pad = "0")
  x <- paste0(prefix, x)
  x
}
