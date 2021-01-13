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
#' Return 'increased' vs. 'decreased' if value is positive vs. negative.
#' @description
#' Return 'increased' vs. 'decreased' if value is positive vs. negative (in R Markdown documents)
#' @param x  number
#' @examples
#' \dontrun{
#' increase_decrease(-0.2)
#' }
#' @export
increase_decrease <- function(x) {
  ifelse(x < 0, "decreased", "increased")
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
NULL
#' @title {Convert numbers to German words}
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
    stop("The absolute value must be less than 10!")

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
#' @title {Plot a colour wheel}
#' @description {Plot a colour wheel and print colours to console}
#' @param term Term the colour name should include
#' @param cex Size of labels
#' @references
#' https://www.r-bloggers.com/colour-wheels-in-r/
#' @export
col_wheel <- function(term = "red", cex = 0.7) {
  cols <- colors()[grep(term, colors())]
  pie(rep(1, length(cols)), labels = cols, col = cols, cex = cex)
  cols
}
NULL