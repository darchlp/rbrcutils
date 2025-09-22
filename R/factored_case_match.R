#' Automatic factoring of dplyr::case_match
#'
#' @param .x A vector to match against
#' @param ... A sequence of two sided formulas: `old_values ~ new_value`. Can either be predefined and reused by use of `!!!lvl` called each time.
#' @param .default The value used when values in .x aren't matched by any of the LHS inputs. If NULL, the default, a missing value will be used.
#'
#' @returns A vector of type factor with the levels in the factor in the order they were put in the list of formulas.
#'
#' @import dplyr
#' @import purrr
#' @importFrom magrittr %>%
#' @import rlang
#' 
#' @export
#' @examples
#' df <- data.frame(
#'   a = c(1, 3, NA, NA, 5, 2),
#'   b = c(2, 3, NA, 3, 1, 4)
#' )
#'
#' lvl <- rlang::exprs(
#'   4 ~ "Large",
#'   3 ~ "Medium",
#'   2 ~ "Small",
#'   1 ~ "Very small"
#' )
#'
#' dplyr::mutate(
#' df,
#'     a = factored_case_match(
#'       a,
#'       !!!lvl
#'     ),
#'     b = factored_case_match(
#'       b,
#' 1 ~ "Very small",
#' 2 ~ "Small",
#' 3 ~ "Medium",
#'       4 ~ "Large"
#'     )
#'   )
factored_case_match <- function(.x, ..., .default = NULL) {
  args <- rlang::list2(...)

  rhs <- purrr::map(args, f_rhs)

  cases <- dplyr::case_match(
    .x,
    !!!args,
    .default = .default
  )

  rlang::exec(forcats::fct_relevel, cases, !!!rhs)
}
