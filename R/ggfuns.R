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
#' @title {Plot histogram with some additional statistics using ggplot2}
#' @description {Plot histogram with some additional statistics using ggplot2.} 
#' Ported from the OurTools package written by Dirk Hasenclever.
#' @param data  data.frame
#' @param x numeric variable (part of data)
#' @param x_lab Label for x-axis (character)
#' @param title Plot title (character)
#' @param title x-intercept for vertical line (numeric)
#' @param scale Scale transformation (character); defaults to "identity" (no transformation)
#' @param theme ggplot-theme (function)
#' @examples
#' \dontrun{
#' gg_nice_hist(data = mtcars, 
#'              x = hp, 
#'              x_lab = 'Gross horsepower',
#'              title = 'mtcars: Gross horsepower',
#'              scale = 'identity',
#'              theme = mdthemes::md_theme_bw())
#' }
#' @import ggplot2
#' @importFrom dplyr pull `%>%`
#' @importFrom tibble rownames_to_column
#' @importFrom modeest mlv
#' @export
gg_nice_hist <- function(data, 
                         x, 
                         x_lab = NULL, 
                         title = NULL, 
                         vline = NULL,
                         scale = "identity", 
                         theme = mdthemes::md_theme_bw()) {

  x_lab <- if (is.null(x_lab)) deparse(substitute(x)) else x_lab
  x <- pull(data, {{ x }})

  df.temp <- data.frame(
    Mode = round(mlv(x, na.rm = TRUE, method = 'shorth'), 2),
    Median = round(median(x, na.rm = TRUE), 2),
    Mean = round(mean(x, na.rm = TRUE), 2)
  )
  NVALID <- sum(!is.na(x))
  NMISSING <- sum(is.na(x))
  MEAN = round(mean(x, na.rm = TRUE), 1)
  SD <- round(sd(x, na.rm = TRUE), 1)
  MAD <- round(mad(x, na.rm = TRUE), 1)
  MEDIAN = round(median(x, na.rm = TRUE), 1)
  IQR <- paste(round(quantile(x, na.rm = TRUE), 1)[2],
    round(quantile(x, na.rm = TRUE), 1)[4],
    sep = " to "
  )
  MODE = round(mlv(x, na.rm = TRUE, method = 'shorth'), 1)

  df.stats <- data.frame(
    x = t(df.temp),
    y = 0,
    col = c("#919C4C", "#C03728", "#FD8F24")
  ) %>%
    rownames_to_column("measure")

  ggplot(df.stats, aes(x = x, y = y, colour = measure, shape = measure)) +
    labs(
      colour = NULL, shape = NULL,
      x = x_lab,
      y = "Density",
      title = title,
      subtitle = paste0("__N (valid)__ = ", NVALID, ", ",
                       "__N (missing)__ = ", NMISSING, ", ",
                       "__Mean (SD)__ = ", paste0(MEAN, ' (', SD, '), '),
                       "__Median (IQR)__ = ", paste0(MEDIAN, ' (', IQR, '), '),
                       "__Mode__ = ", MODE)
    ) +
    scale_colour_manual(values = as.character(df.stats$col)) +
    scale_fill_manual(values = as.character(df.stats$col)) +
    scale_shape_manual(values = c(17, 15, 16)) +
    geom_rug(aes(x = x), sides = "t", inherit.aes = FALSE, data = data) +
    geom_vline(xintercept = vline, colour = 'red', linetype = 4) +
    stat_bin(aes(x = x, y = ..density..),
      data = data, inherit.aes = FALSE,
      bins = 30,
      color = "black",
      fill = "#dfe3ee",
      size = 0.2,
      alpha = 0.5
    ) +
    geom_line(
      stat = "density", data = data, aes(x = x), inherit.aes = FALSE,
      size = 1.2, colour = "#3b5998"
    ) +
    geom_point(size = 5) +
    theme +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "bottom"
   #   plot.caption = element_markdown(hjust = 0)
    ) +
    scale_x_continuous(trans = scale) # 'identity', 'log10', etc.
}
NULL