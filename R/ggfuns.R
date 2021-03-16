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
#' @export
ggnice_hist <- function(data, 
                         x, 
                         x_lab = NULL, 
                         x_breaks = NULL,
                         title = NULL, 
                         vline = NULL,
                         digits = 1,
                         scale = scales::identity_trans(), 
                         theme = mdthemes::md_theme_bw()) {

  x_lab <- if (is.null(x_lab)) deparse(substitute(x)) else x_lab
  x <- pull(data, {{ x }})
  x <- as.numeric(x)
  x_breaks <- if (is.null(x_breaks)) waiver() else x_breaks

  transformer <- if (scale[[1]] == 'identity') 'none' else scale[[1]]
  
  mode_fun <- function(x) {
    d <- stats::density(x, na.rm = TRUE)
    d$x[which.max(d$y)]
  }

  df.temp <- data.frame(
    Mode = round(mode_fun(x), digits),
    Median = round(median(x, na.rm = TRUE), digits),
    Mean = round(mean(x, na.rm = TRUE), digits)
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
    scale_x_continuous(trans = scale, breaks = x_breaks) 
}
NULL
#' @title {Plot normal distribution using ggplot2}
#' @description {Plots normal distribution along with standard deviations 
#' and confidence intervals using ggplot2.} 
#' @param line_color  Colour of the vertical lines
#' @param fill_color Colour of the area under the curve
#' @param arrow_color Colour of the arrows
#' @import ggplot2
#' @importFrom grid arrow
#' @export
ggnorm_dist <- function(line_color = 'white',
                        fill_color = 'steelblue',
                        arrow_color = 'black') {
  
  arrow <- grid::arrow(length = unit(5, "mm"), angle = 15)

  ggplot(data.frame(x = c(-3.5, 0,  3.5)), aes(x)) +
    stat_function(
      fun = dnorm,
      geom = "line"
    ) +
    stat_function(
      fun = dnorm,
      geom = "area",
      fill = fill_color,
      xlim = c(-3, 3),
      alpha = .3
    ) + 
    stat_function(
      fun = dnorm,
      geom = "area",
      fill = fill_color,
      xlim = c(-2, 2),
      alpha = .5
    ) +
    stat_function(
      fun = dnorm,
      geom = "area",
      fill = fill_color,
      xlim = c(-1, 1),
      alpha = .8
    ) +
    scale_x_continuous(breaks = seq(-3, 3, 1)) +
    scale_y_continuous(expand = c(0, 0.01)) +
    geom_vline(xintercept = seq(-2, 2, 1), colour = line_color, linetype = 2, size = 1) +
    geom_segment(x = rep(0, 3), xend = c(3, 2, 1), 
                 y = c(0, 0.05, 0.24), yend = c(0, 0.05, 0.24), 
                 color = arrow_color, size = 1, arrow = arrow) +
    geom_segment(x = rep(0, 3), xend = c(-3, -2, -1), 
                 y = c(0, 0.05, 0.24), yend = c(0, 0.05, 0.24), 
                 color = arrow_color, size = 1, arrow = arrow) +
    geom_label(data = data.frame(x = rep(0, 3), 
                                 y = c(0, 0.05, 0.24),
                                 label = c('99%', '95%', '68%')),
               aes(x = x, y = y, label = label), size = 7) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 18),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = 20)) +
    labs(x = 'Standard deviations around sample mean', y = NULL)
}
NULL
#' @title {Modulus scale transformation}
#' @description {Modulus scale transformation function for ggplot2} 
#' @source {\url{http://freerangestats.info/blog/2015/09/05/creating-a-scale-transformation}}
#' @param lambda  Tuning parameter
#' @export
modulus_trans <- function(lambda) {
  scales::trans_new("modulus",
                    transform = function(y) {
                      if (lambda != 0) {
                        yt <- sign(y) * (((abs(y) + 1)^lambda - 1) / lambda)
                      } else {
                        yt <- sign(y) * (log(abs(y) + 1))
                      }
                      return(yt)
                    },
                    inverse = function(yt) {
                      if (lambda != 0) {
                        y <- ((abs(yt) * lambda + 1)^(1 / lambda) - 1) * sign(yt)
                      } else {
                        y <- (exp(abs(yt)) - 1) * sign(yt)
                      }
                      return(y)
                    }
  )
}
NULL
#' @title {Sliding Incidence Rate}
#' @description {Calculate incidence rate with sliding windows}
#' @param data {A data.frame }
#' @param var_start_date {Variable with first day of episodes (default: ICU_START)}
#' @param var_end_date {Variable with last day of episodes (default: ICU_END))}
#' @param var_event_date {Variable with date of event (NULL)}
#' @param var_grouping {Grouping variable (default: PERIOD)}
#' @param offset {
#' \itemize{
#'  \item \code{start} {Number of days between start of period 
#'  and beginning of time under risk (default: 3)}
#'  \item \code{end} {Number of days between end of period 
#'  and end of time under risk (default: 2)}
#' }
#' }
#' @param time_span {Length of intervention period in days (default: 365)}
#' @param scale_fct {Scaling factor for incidence rate (default: 1.000)}
#' @param window {Size of sliding window in days (default: 20)}
#' @param step {Step between sliding windows in days (default: 3)}
#' @return {A data.frame with sliding incidence rates}
#' @author {Dirk Hasenclever, Norbert Koehler}
#' @export
sliding_IR <- function(data,
                       var_start_date = ICU_START,
                       var_end_date = ICU_END,
                       var_event_date = NULL,
                       var_grouping = PERIOD,
                       offset = list(start = 3, end = 2),
                       time_span = 365,
                       scale_fct = 1000,
                       window = 20,
                       step = 3) {
  var_start_date <- enquo(var_start_date)
  var_end_date <- enquo(var_end_date)
  var_event_date <- enquo(var_event_date)
  group_name <- deparse(substitute(var_grouping))
  var_grouping <- enquo(var_grouping)
  group_categories <- dplyr::pull(data, {{ var_grouping }}) %>% unique()
  num_categories <- length(group_categories)


  data <- data %>%
    # var_start_date & var_end_date must not by missing
    filter(!is.na({{ var_start_date }}) & !is.na({{ var_end_date }})) %>%
    # Add offset to get periods at risk
    mutate_at(vars({{ var_start_date }}), list(~ lubridate::days(offset$start) + .)) %>%
    mutate_at(vars({{ var_end_date }}), list(~ lubridate::days(offset$end) + .))

  df.temp1 <- data %>%
    group_by({{ var_grouping }}) %>%
    mutate(
      days_since_start_1 = as.numeric(as.Date({{ var_start_date }}) - min((as.Date({{ var_start_date }})))),
      days_since_start_2 = as.numeric(as.Date({{ var_end_date }}) - min((as.Date({{ var_start_date }})))),
      days_since_event = as.numeric(as.Date({{ var_event_date }}) - min((as.Date({{ var_start_date }})))),
      days_since_start_2 = ifelse(!is.na({{ var_event_date }}), days_since_event, days_since_start_2)
    ) %>%
    rowwise() %>%
    mutate(ICU = list(days_since_start_1:days_since_start_2)) %>%
    arrange({{ var_grouping }}, {{ var_start_date }}) %>%
    select({{ var_grouping }}, starts_with("days_since"), ICU) %>%
    group_by({{ var_grouping }}) %>%
    summarise(
      #   DAY_FIRST = min(ICU_START, na.rm=TRUE),
      TIME_AT_RISK = list(unlist(ICU)),
      NUMBER_OF_EVENTS = list(days_since_event)
    ) %>%
    ungroup()

  df.temp2 <- tibble(
    WINDOW_START = rep(seq(0, time_span - window, by = step), num_categories),
    WINDOW_END = WINDOW_START + window,
    group = rep(group_categories, each = length(WINDOW_START) / num_categories)
  ) %>%
    rename_at(vars(3), list(~ c(group_name)))



  df.RESULTS <- df.temp2 %>%
    left_join(df.temp1, by = group_name) %>%
    rowwise() %>%
    mutate(WINDOW = list(WINDOW_START:WINDOW_END)) %>%
    mutate(
      EVENTS = sum(NUMBER_OF_EVENTS %in% WINDOW),
      DAYS_AT_RISK = sum(TIME_AT_RISK %in% WINDOW),
      IR = EVENTS / DAYS_AT_RISK * scale_fct
    ) %>%
    select(-c(TIME_AT_RISK, NUMBER_OF_EVENTS, WINDOW))
}
NULL


