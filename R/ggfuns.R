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
#' @param vline x-intercept for vertical line (numeric)
#' @param digits Number of digits (numeric)
#' @param scale Scale transformation (character); defaults to "identity" (no transformation)
#' @param theme ggplot-theme (function)
#' @examples
#' \dontrun{
#' ggnice_hist(data = mtcars, 
#'              x = hp, 
#'              x_lab = 'Gross horsepower',
#'              title = 'mtcars: Gross horsepower',
#'              scale = 'identity',
#'              theme = mdthemes::md_theme_bw())
#' }
#' @import ggplot2
#' @import scales
#' @importFrom dplyr pull `%>%`
#' @importFrom tibble rownames_to_column
#' @importFrom modeest mlv
#' @export
ggnice_hist <- function(data, 
                         x, 
                         x_lab = NULL, 
                         title = NULL, 
                         vline = NULL,
                         digits = 1,
                         scale = scales::identity_trans(), 
                         theme = mdthemes::md_theme_bw()) {

  x_lab <- if (is.null(x_lab)) deparse(substitute(x)) else x_lab
  x <- pull(data, {{ x }})

  transformer <- if (scale[[1]] == 'identity') 'none' else scale[[1]]

  df.temp <- data.frame(
    Mode = round(mlv(x, na.rm = TRUE, method = 'shorth'), 2),
    Median = round(median(x, na.rm = TRUE), 2),
    Mean = round(mean(x, na.rm = TRUE), 2)
  )
  NVALID <- sum(!is.na(x))
  NMISSING <- sum(is.na(x))
  MEAN = format_num(mean(x, na.rm = TRUE), digits)
  SD <- format_num(sd(x, na.rm = TRUE), digits)
  MAD <- format_num(mad(x, na.rm = TRUE), digits)
  MEDIAN = format_num(median(x, na.rm = TRUE), digits)
  IQR <- paste(format_num(quantile(x, na.rm = TRUE), digits)[2],
               format_num(quantile(x, na.rm = TRUE), digits)[4],
    sep = " to "
  )
  mode_fun <- function(x) {
    d <- stats::density(x, na.rm = TRUE)
    d$x[which.max(d$y)]
  }
  MODE = format_num(mode_fun(x), digits)

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
      caption = paste('x-scale transformer:', transformer),
      subtitle = paste0("__N (valid)__ = ", NVALID, ", ",
                       "__N (missing)__ = ", NMISSING, ", ",
                       "__Mean (SD)__ = ", paste0(MEAN, ' (', SD, '), '),
                       "__Median (IQR)__ = ", paste0(MEDIAN, ' (', IQR, '), '),
                       "__Mode__ = ", MODE)
    ) +
    scale_colour_manual(values = as.character(df.stats$col)) +
    scale_fill_manual(values = as.character(df.stats$col)) +
    scale_shape_manual(values = c(17, 15, 16)) +
    stat_bin(aes(x = x, y = ..density..),
             data = data, inherit.aes = FALSE,
             bins = 30,
             color = "black",
             fill = "#dfe3ee",
             size = 0.2,
             alpha = 0.5
    ) +
    geom_rug(aes(x = x), sides = "t", inherit.aes = FALSE, data = data) +
    geom_vline(xintercept = vline, colour = 'red', linetype = 4, size = 1) +
    geom_line(
      stat = "density", data = data, aes(x = x), inherit.aes = FALSE,
      size = 1.2, colour = "#2c3e50"
    ) +
    geom_point(size = 5) +
    theme +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "bottom"
    ) +
    scale_x_continuous(trans = scale) 
}
NULL
#' @title {Plot normal distribution using ggplot2}
#' @description {Plot normal distribution along with standard deviations 
#' and confidence intervals using ggplot2.} 
#' Modified version of stevemisc::normal_distribution()
#' @source {\url{https://github.com/svmiller/stevemisc}}
#' @param curvecolor  Colour of the curve
#' @param fillcolor Colour of the area under the curve
#' @param arrowcolor Colour of the arrows
#' @examples
#' \dontrun{
#' ggnorm_dist()
#' }
#' @import ggplot2
#' @export
ggnorm_dist <- function(curvecolor = "grey10", 
                        fillcolor = "#C7271C",
                        arrowcolor = 'yellow') {
  ggplot(data.frame(x = c(-4, 4)), aes(x)) +
    stat_function(
      fun = dnorm,
      xlim = c(-1, 1), size = 0,
      geom = "area", fill = fillcolor, alpha = 0.5
    ) +
    geom_vline(xintercept = 0, colour = "white", size = 1) +
    stat_function(
      fun = dnorm, xlim = c(
        qnorm(0.025),
        abs(qnorm(0.025))
      ), size = 0, geom = "area",
      fill = fillcolor, alpha = 0.4
    ) +
    stat_function(
      fun = dnorm,
      xlim = c(qnorm(0.005), abs(qnorm(0.005))), size = 0,
      geom = "area", fill = fillcolor, alpha = 0.3
    ) +
    geom_segment(x = 1, y = 0, xend = 1, yend = dnorm(
      1,
      0, 1
    ), color = "white", linetype = "dashed") +
    geom_segment(x = -1, y = 0, xend = -1, yend = dnorm(
      1,
      0, 1
    ), color = "white", linetype = "dashed") +
    geom_segment(x = abs(qnorm(0.025)), y = 0, xend = abs(qnorm(0.025)), yend = dnorm(
      1,
      0, 1
    ), color = "white", linetype = "dashed") +
    geom_segment(x = qnorm(0.025), y = 0, xend = qnorm(0.025), yend = dnorm(
      1,
      0, 1
    ), color = "white", linetype = "dashed") +
    geom_segment(
      x = -0.15,
      y = 0.2, xend = -0.99, yend = 0.2,
      color = arrowcolor,
      size = 1,
      arrow = arrow(length = unit(0.15, "cm"))
    ) +
    geom_segment(
      x = 0.15, y = 0.2, xend = 0.99, yend = 0.2,
      color = arrowcolor,
      size = 1,
      arrow = arrow(length = unit(
        0.15,
        "cm"
      ))
    ) +
    geom_segment(
      x = -0.15,
      y = 0.05, xend = -1.95, yend = 0.05,
      color = arrowcolor,
      size = 1,
      arrow = arrow(length = unit(0.15, "cm"))
    ) +
    geom_segment(
      x = 0.15, y = 0.05, xend = 1.95, yend = 0.05,
      color = arrowcolor,
      size = 1,
      arrow = arrow(length = unit(
        0.15,
        "cm"
      ))
    ) +
    geom_segment(
      x = -0.15,
      y = 0.01, xend = -2.57, yend = 0.01,
      color = arrowcolor,
      size = 1,
      arrow = arrow(length = unit(0.15, "cm"))
    ) +
    geom_segment(
      x = 0.15, y = 0.01, xend = 2.57, yend = 0.01,
      color = arrowcolor,
      size = 1,
      arrow = arrow(length = unit(
        0.15,
        "cm"
      ))
    ) +
    annotate(
      geom = "label", x = 0, y = 0.2, label = "68%",
      size = 4.5, color = "black"
    ) +
    annotate(
      geom = "label",
      x = 0, y = 0.05, label = "95%", size = 4.5,
      color = "black"
    ) +
    annotate(
      geom = "label",
      x = 0, y = 0.01, label = "99%", size = 4.5,
      color = "black"
    ) +
    stat_function(
      fun = dnorm,
      color = curvecolor, size = 1.2
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(breaks = c(
      -4,
      -2.58, -1.96, -1, 0, 1, 1.96, 2.58,
      4
    )) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_line(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = "Normal distribution",
      subtitle = "with standard deviations and confidence intervals",
      x = "Standard Deviation"
    )
}
NULL