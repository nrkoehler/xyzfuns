#' @title {Add 'und' or 'and' to Listing}
#' @description Add 'und' or 'and' before the last element of a character vector.
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
#' @title {Print calendar to console}
#' @description {Prints a monthly calendar to the console}
#' @source {\url{https://www.garrickadenbuie.com/blog/r-console-calendar/}}
#' @author {Garrick Aden-Buie}
#' @import dplyr
#' @import lubridate
#' @importFrom crayon silver red bold col_nchar col_align
#' @importFrom tidyr complete nesting
#' @importFrom cli cat_line
#' @export
cal <- function(
  start_date = lubridate::today(),
  end_date = start_date + 28,
  week_start = 1
) {
  `%>%` <- dplyr::`%>%`
  
  if (!inherits(start_date, "Date")) {
    start_date <- lubridate::ymd(start_date, truncated = 1)
  }
  if (!inherits(end_date, "Date")) {
    end_date <- lubridate::ymd(end_date, truncated = 1)
  }
  
  start_date <- lubridate::floor_date(start_date, "month")
  end_date <- lubridate::rollback(lubridate::ceiling_date(end_date, "month"))
  
  tibble::tibble(
    date      = seq(start_date, end_date, by = "day"),
    day       = lubridate::day(date),
    wday      = lubridate::wday(.data$date, label = FALSE, abbr = TRUE, week_start = week_start),
    weekend   = lubridate::wday(.data$date, label = FALSE, week_start = 1) %in% 6:7,
    week      = as.integer(lubridate::floor_date(.data$date, unit = "week", week_start = week_start)),
    month     = lubridate::month(.data$date, label = TRUE, abbr = FALSE),
    month_int = lubridate::month(.data$date, label = FALSE),
    year      = lubridate::year(lubridate::floor_date(.data$date, unit = "year", week_start = week_start))
  ) %>% 
    dplyr::group_by(month, year) %>%
    dplyr::mutate(week = week - min(week) + 1) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(year, month_int, month), wday = 1:7, week) %>%
    dplyr::arrange(year, month_int, week, wday) %>%
    dplyr::mutate(
      day = sprintf("%2s", day),
      day = dplyr::if_else(weekend, as.character(crayon::silver(day)), day),
      day = dplyr::if_else(
        date == lubridate::today(), 
        as.character(crayon::bold(crayon::red(day))),
        day
      ),
      month_label = paste(month, year)
    ) %>%
    tidyr::replace_na(list(day = "  ")) %>%
    dplyr::group_by(year, month_int, month_label, week) %>%
    dplyr::summarize(day = paste(day, collapse = " "), .groups = "drop") %>%
    dplyr::group_by(month_int) %>%
    dplyr::mutate(
      width = max(crayon::col_nchar(day)),
      day = crayon::col_align(day, width = width, align = "right"),
      month_label = crayon::col_align(month_label, width = width, align = "center"),
      month_label = crayon::bold(month_label)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(
      dplyr::distinct(., year, month_int, day = month_label, week = 0)
    ) %>%
    dplyr::mutate(width = max(crayon::col_nchar(day))) %>%
    dplyr::arrange(year, month_int, week) %>%
    dplyr::group_by(year, month_int) %>%
    dplyr::mutate(
      row = dplyr::cur_group_id() - 1,
      row = floor(row / (getOption("width") %/% (width + 2))),
    ) %>%
    dplyr::group_by(row, week) %>%
    dplyr::summarize(text = paste(day, collapse = "    "), .groups = "drop_last") %>%
    dplyr::mutate(text = dplyr::if_else(week == max(week), paste0(text, "\n"), text)) %>%
    dplyr::pull(text) %>%
    cli::cat_line()
}
NULL
#' @title {Compare Two Vectors}
#' @description {Compare two vectors and return intersections.}
#' @source {\url{https://twitter.com/tyluRp/status/1197634755430367235}}
#' @examples
#' \dontrun{
#' x <- 1:4
#' y <- 3:6
#' compare(x, y)
#' }
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
#' @title {Plot a colour wheel}
#' @description {Plot a colour wheel and print colours to console}
#' @param term Term the colour name should include
#' @source {\url{https://www.r-bloggers.com/colour-wheels-in-r/}}
#' @export
col_wheel <- function (term = "red", cex = 0.7) 
{
  cols <- colors()[grep(term, colors())]
  pie(rep(1, length(cols)), labels = cols, col = cols, cex = cex)
  cols
}
NULL
#' @title {Fix variable names with encoding errors}
#' @description {Fix variable names with encoding errors.}
#' @export
fix_names <- function(x) {
  names(x) <- enc2native(names(x))
  x
}
NULL
#' @title {Format numbers}
#' @description {Format numbers (in R Markdown documents)}
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
#' @title {Display the top and bottom of a data frame}
#' @description {Quickly display the top and bottom of a data frame}
#' @param data data frame
#' @param n number of rows to display from head and tail
#' \dontrun{
#' ht(mtcars)
#' }
#' @export
ht <- function(data, n = 5){
  rbind(head(data, n), tail(data, n))
}
NULL
#' @title {Return 'increased' vs. 'decreased' if value is positive vs. negative}
#' @description
#' Return 'increased' vs. 'decreased' if value is positive vs. negative (in R Markdown documents)
#' @param x  number
#' @export
increase_decrease <- function(x) {
  ifelse(x < 0, "decreased", "increased")
}
NULL
#' @title {Put objects of global environment into a list}
#' @description {Put objects of global environment 
#' matching a pre-defined pattern into a list}
#' @param pattern Pattern to search for; may be a 
#' regular expression
#' @export
globenv_2_list <- function(pattern = '^df') {
  
  namen <- ls(pattern = pattern,
              envir = .GlobalEnv)
  
  l <- length(namen)
  
  obj.lst <- vector(mode = "list", length = l)
  
  for (i in 1:l) {
    obj.lst[i] <- ls(pattern = pattern)[i]
    obj.lst <- setNames(object = obj.lst, nm = namen)
    obj.lst[i] <- get(namen[i], envir = .GlobalEnv)
  }
  obj.lst
}
NULL
#' @title {Print estimate, 95\% CI, and p-value of regression modles}
#' @description {Print estimate, 95\% CI, and p-value as derived from broom::tidy\(fit\)}
#' @param data data frame as derived from broom::tidy\(fit\)
#' @param beta estimate
#' @param unit unit of the estimate \(needs to be quoted\)
#' @param lower Lower bound of 95\% CI
#' @param upper Upper bound of 95\% CI
#' @param p p-value
#' @param term Name of independent variable \(needs to be quoted, partial match is sufficient\)
#' @param filter_var Name of variable for which a filter should be applied
#' @param filter_value Term to be filtered \(needs to be quoted\)
#' @param digits Number of digits for estimate and CI \(default: 2\)
#' 
#' \dontrun{
#' model_2_txt(df)
#' }
#' @import dplyr
#' @importFrom scales pvalue
#' @export
model_2_txt <- function(data,
                        beta = estimate,
                        unit = "",
                        lower = conf.low,
                        upper = conf.high,
                        p = p.value,
                        term,
                        filter_var,
                        filter_value,
                        digits = 2) {
  data <- data %>%
    filter(
      str_detect(term, {{ term }}),
      {{ filter_var }} == filter_value
    ) %>%
    mutate_at(
      vars({{ beta }}, {{ lower }}, {{ upper }}),
      list(~ xyzfuns::format_num(., digits = digits))
    ) %>%
    mutate(p := {{ p }},
           p = scales::pvalue(p, add_p = TRUE)
    )
  
  beta <- data %>% pull({{ beta }})
  lower <- data %>% pull({{ lower }})
  upper <- data %>% pull({{ upper }})
  p <- data %>% pull(p)
  paste0("Beta = ", beta, unit, ", 95% CI [", lower, " to ", upper, "], ", p)
}
NULL
#' @title {Return 'more' vs. 'less' if value is positive vs. negative}
#' @description {Return 'more' vs. 'less' if value is positive vs. negative 
#' (in R Markdown documents)}
#' @param x  number
#' @param rev  FALSE (default) for increasing trend, TRUE for decreasing trend (reduction).
#' @export
more_less <- function(x, rev = FALSE) {
  if (rev == FALSE) {
    ifelse(x < 0, "less", "more")
  } else {
    ifelse(x < 0, "more", "less")
  }
}
NULL
#' @title {Named {group_split()} function}
#' @description {Modified version of {dplyr::group_split()}. 
#' Unlike the original function, it returns named lists.}
#' @source {\url{https://github.com/tidyverse/dplyr/issues/4223}}
#' @param .tbl Grouping variable
#' @import dplyr
#' @importFrom rlang eval_bare set_names
#' @export
named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " / ")))

  grouped %>%
    group_split() %>%
    rlang::set_names(names)
}
NULL
#' @title {Convert numbers to German words}
#' @description {Convert numbers from 0 to 10 to German words.}
#' @param x A numeric vector. Values should be integers. The absolute values should be between 0 and 10.
#' @param cap Whether to capitalize the first letter of the word. This can be useful when the word is at the 
#' beginning of a sentence. Default is FALSE.
#' @param female Logical. Make gender of "ein" female ("eine").
#' @export
n2w_de <- function(x, cap = FALSE, female = FALSE) {
  if (!is.numeric(x)) {
    stop("The input is not numeric.")
  }
  x = abs(x)
  if (any(abs(x) > 10)) 
    stop("The absolute value must not be greater than 10!")

  if (any(x != floor(x))) {
    stop("The numbers must be integer.")
  }
  
x <- ifelse(x == 0, 'null',
       ifelse(x == 1, 'ein',
              ifelse(x == 2, 'zwei',
                     ifelse(x == 3, 'drei',
                            ifelse(x == 4, 'vier',
                                   ifelse(x == 5, 'fünf',
                                          ifelse(x == 6, 'sechs',
                                                 ifelse(x == 7, 'sieben',
                                                        ifelse(x == 8, 'acht',
                                                               ifelse(x == 9, 'neun',
                                                                      ifelse(x == 10, 'zehn', '')))))))))))
if (female == TRUE) x = sub('ein', 'eine', x)
if (cap == TRUE) x = sub("^([a-z])", "\\U\\1", x, perl = TRUE)
x
  
  
}
NULL
#' @title {Add Prefix and Leading Zero(s) to Variables}
#' @description {Add prefix and leading zero(s) to variables in order to get strings of equal length.}
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
NULL
#' @title {Print 'Lorem Ipsum'}
#' @description {Print 'Lorem Ipsum' to the console or
#' in R Markdown documents}
#' @param n The number of lorem ipsum paragraphs to be printed
#' @source {\url{https://github.com/aakosm/lipsum}}
#' @export
paste_lipsum <- function(n = 1){
  ipsum <- paste("Lorem ipsum dolor sit amet, consectetur adipiscing elit,",
                 "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.", 
                 "Ut enim ad minim veniam, quis nostrud exercitation ullamco",
                 "laboris nisi ut aliquip ex ea commodo consequat.", 
                 "Duis aute irure dolor in reprehenderit in voluptate velit",
                 "esse cillum dolore eu fugiat nulla pariatur. Excepteur sint",
                 "occaecat cupidatat non proident, sunt in culpa qui officia",
                 "deserunt mollit anim id est laborum.")
  for (i in 1:n) {
    cat(ipsum, sep = "\n")
  }
}
NULL
#' @title {Estimate rate ratio}
#' @description {Estimate rate ratio along with confidence interval
#' and p-value.}
#' @details {Tidy version of Minato Nakazawa's fmsb::rateratio() function.}
#' @param a The number of events in group a.
#' @param b The number of events in group b.
#' @param time.a Time under risk of group a.
#' @param time.b Time under risk of group b.
#' @param conf.level Confidence level (default = 0.95)
#' @return The function returns a data.frame with four variables:
#' \itemize{
#'  \item \code{EST} {Point estimate}
#'  \item \code{CIL} {Confidence interval, lower bound}
#'  \item \code{CIU} {Confidence interval, upper bound}
#'  \item \code{PVAL} {p-Value}
#' }
#' @importFrom scales pvalue
#' @source {\url{https://mirror.linux.duke.edu/cran/web/packages/fmsb/index.html}}
#' @export
rate_ratio <- function(a, b, time.a, time.b, conf.level = 0.95) {
  .M <- a + b
  .T <- time.a + time.b
  .MAT <- matrix(c(a, b, .M, time.a, time.b, .T), 3, 2)
  ESTIMATE <- (a / time.a) / (b / time.b)
  norm.pp <- qnorm(1 - (1 - conf.level) / 2)
  .CHI <- (a - (time.a / .T) * .M) / sqrt(.M * (time.a / .T) * (time.b / .T))

  estimates <- data.frame(
    EST = (a / time.a) / (b / time.b),
    CIL = ESTIMATE * exp(-norm.pp * sqrt(1 / a + 1 / b)),
    CIU = ESTIMATE * exp(norm.pp * sqrt(1 / a + 1 / b)),
    PVAL = scales::pvalue(2 * (1 - pnorm(abs(.CHI))))
  )
}
NULL
#' @title {Scaling by two standard deviations}
#' @description {Scale a numeric vector by by two standard deviations
#' (rather than by one)}
#' @param x Numeric vector
#' @export
scale_by_2sd <- function(x) {
  scale(x, center = TRUE, scale = FALSE) / (2 * sd(x, na.rm = TRUE))
}
NULL
#' @title {Show data.frame in MS Excel}
#' @description {Show data.frame in MS Excel and continue piping}
#' @source {\url{https://twitter.com/CorradoLanera/status/1447478650488737792}}
#' @param .data data.frame
#' @importFrom openxlsx write.xlsx
#' @importFrom fs file_show
#' @export
show_in_excel <- function(.data) {
  if (interactive()) { # avoid unwanted Excel executions
    tmp <- tempfile(fileext = '.xlsx') 
    openxlsx::write.xlsx(.data, tmp)
    fs::file_show(tmp)
  }
  .data # so that we can continue piping
}
NULL
#' @title {Shorten character string}
#' @description {Shortens a character string to a specified length}
#' @param x character string to be shortened
#' @param l_max maximal length of character string (numeric), default = 10
#' @examples
#' \dontrun{str_abbrev('Hypertension', l_max = 6)}
#' @export
str_abbrev <- function(x, l_max = 10) {
  ifelse(nchar(x) > l_max, paste0(substr(x, 1, l_max), "."), x)
}
NULL
#' @title {Tranform a character vector into a data frame}
#' @description {A character vector can be printed into a bulleted list.
#' However, a long list may be rather space consuming. This function transforms
#' a character vector into a data frame, which may be printed as a table.}
#' @param x Character vector
#' @param n_col Desired number of columns for the data frame
#' @param order Shall the character vector be ordered horizontally 
#' ('horizontal', default) or vertically ('vertical')?
#' @export
str_2_df <- function(x, n_col = 6, order = 'horizontal'){
  
  if (length(x) < n_col) stop('Number of columns must not be smaller than the number of elements in the character vector!')
  
  if (order == 'horizontal') {
    n_row <- n_col
    n_col <- ceiling(length(x) / n_row)
  } else if (order == 'vertical') {
    n_row <- ceiling(length(x) / n_col)
  } else {
    stop("'order' must be either 'horizontal' or 'vertical'. Please check for typos!")
  }
  
  data <- data.frame(matrix(ncol = n_col, nrow = n_row))
  colnames(data) <- LETTERS[1:n_col]
  data$A <- x[1:n_row]
  for (i in 2:n_col) {
    data[, i] <- x[((i-1)*n_row+1):(i*n_row)]
  }
  
  data[,n_col] <- ifelse(is.na(data[,n_col]), '', data[,n_col])
  
  if (order == 'horizontal') {
    data <- t(data)
    data <- as.data.frame(data)
    row.names(data) <- NULL
  } else if (order == 'vertical') {
    data <- data
  } else {
    stop("'order' must bei either 'horizontal' or 'vertical'. Please check for typos!")
  }
  data
}
#' @title {Character string to bullet list}
#' @description {Convert a character string to a bullet list}
#' @export
str_2_list <- function(str){
  cat(paste('-', str), sep = '\n') 
}
NULL
#' @title {Update Packages fast}
#' @description {Update Packages with the following defaults: ask=F, Ncpus=6}
#' @export
update_fast <- function(){utils::update.packages(ask=FALSE, Ncpus=6)}
NULL
#' @title {Prepare date variables for export}
#' @description {Prepare date variables for export to Excel, SPSS, etc.}
#' @export
write_dates <- function(x) {
  y <- nchar(as.character(x[which(!is.na(x))[1]]))
  y <- ifelse(is.na(y), 1, y)
  x <- if (y == 10) {
    lubridate::ymd(x)
  } else if (y == 16) {
    lubridate::ymd_hm(x)
  } else if (y == 19) {
    lubridate::ymd_hms(x)
  } else {
    x <- x
  }
}
NULL


