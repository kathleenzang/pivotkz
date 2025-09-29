#' Convert a dataset from wide to long format
#'
#' @param data A data frame or tibble containing the data to reshape.
#' @param cols Columns to pivot into long format (e.g., `starts_with("X")` or `2:10`).
#' @param value_name A string giving the name of the new value column in the long dataset.
#' @param names_to A string. The name for the column that stores the original column names. Default is `"year"`.
#' @param prefix A string prefix to strip from column names before converting them to numeric. Default is `"X"`.
#'
#' @returns A tibble in long format, with identifier columns preserved, and the
#'   selected `cols` pivoted into two columns: one named by `names_to`, and
#'   one named by `value_name`.
#'
#' @export
#'
#' @importFrom dplyr %>% mutate
#'
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#'
#' # Example 1: Simple dataset
#' df <- tibble(
#'   country = c("A", "B"),
#'   X2000 = c(1, 2),
#'   X2001 = c(3, 4)
#' )
#' pivotkz(df, starts_with("X"), "value")
#'
#' # Example 2: Using the maternal_mortality dataset included in the package
#' data(maternal_mortality)
#' head(maternal_mortality)
#' long_mm <- pivotkz(
#'   data = maternal_mortality,
#'   cols = starts_with("X"),
#'   value_name = "mortality_ratio"
#' )
#' head(long_mm)

pivotkz <- function(data, cols, value_name, names_to = "year", prefix = "X") {
  data %>%
    pivot_longer(
      cols = {{ cols }},
      names_to = names_to,
      values_to = value_name
    ) %>%
    mutate(
      !!names_to := as.numeric(sub(prefix, "", .data[[names_to]]))
    )
}
