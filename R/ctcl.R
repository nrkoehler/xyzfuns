#' @title {Secutrial: Mapping variable names to variable labels}
#' @description {Mapping the variable names of a table from SecuTrial to variable labels}
#' @return The function returns a tibble with 2 variables:
#'  \itemize{
#'  \item \code{ITEM_COLUMN:} {Variable name}
#'  \item \code{ITEM_LABEL:} {Variable label}
#' }
#' @examples
#' \dontrun{
#' df <- map_varlabs(modul = 'mnppal_heart')
#' }
#' @param modul Name of table from SecuTrial database
#' @import dplyr
#' @export
st_map_varlabs <- function(modul) {

    df <- tibble(ITEM_COLUMN = c('PATIENT_ID', 'VISIT_LABEL', 'PAGE_LABEL'),
               ITEM_LABEL = c('Patienten_ID', 'Visite', 'Modul')) %>%
    mutate(ITEM_COLUMN = ifelse(ITEM_COLUMN != "PATIENT_ID",
                                paste(modul, ITEM_COLUMN, sep = "_"),
                                ITEM_COLUMN
    ))
  ITEMDESC <- ITEMDESC %>%
    filter(PAGE_TABLE == modul) %>%
    mutate(
      ITEM_COLUMN = str_to_upper(ITEM_COLUMN),
      ITEM_COLUMN = paste(PAGE_TABLE, ITEM_COLUMN, sep = "_")
    ) %>%
    select(ITEM_COLUMN, ITEM_LABEL) %>%
    distinct() # %>%
  #  mutate(ITEM_COLUMN = str_to_upper(ITEM_COLUMN))
  df %>%
    bind_rows(ITEMDESC)
}
NULL