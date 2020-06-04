#' @title Logarithmic transformation of x-scale
#' @description Logarithmic transformation of x-scale in ggplot graphics.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg)) + geom_density() + scale_x_ln2()
#' }
#' @export
scale_x_ln2 <- function(...){
  scale_x_continuous(..., trans = scales::log_trans())
}
NULL
#' @title Logarithmic transformation of y-scale
#' @description Logarithmic transformation of y-scale in ggplot graphics.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg)) + geom_density() + scale_y_ln2()
#' }
#' @export
scale_y_ln2 <- function(...){
  scale_x_continuous(..., trans = scales::log_trans())
}
NULL