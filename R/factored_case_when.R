#' Automatic factoring of dplyr::case_when
#'
#' @param ... A sequence of two sided formulas: `logical_test ~ new_value`. 
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
#'   x = c(1, 2, 3, 4, NA),
#'   y = c(NA, 5, 4, 3, 2)
#' )
#'
#' dplyr::mutate(
#' df,
#'     x_label = factored_case_when(
#'       x == 4 ~ "A pig",
#'       x == 3 ~ "Medium",
#'       x == 2 ~ "Three",
#'       x == 1 ~ "Very small",
#'       is.na(x) ~ "Missing"
#'     ),
#'    y_label = factored_case_when(
#' y >= 4 ~ "High",
#' .default = "Low"
#' )
#'   )
factored_case_when <- function(..., .default = NULL) {
  args <- rlang::enquos(...)

  # Evaluate the case_when logic
  result <- dplyr::case_when(
    !!!args,
    .default = .default
  )

  # Extract the RHS values in order
  rhs_vals <- purrr::map_chr(args, ~ as.character(rlang::quo_get_expr(.x)[[3]]))

  # Remove any duplicated values while preserving order
  rhs_levels <- unique(rhs_vals)

  # Turn into a factor with desired levels
  factor(result, levels = rhs_levels)
}
