#' Plot a categorical variable over a grouping variable.
#'
#' @param .df A data.frame or tibble that contains a categorical and grouping variable.
#' @param xvar The categorical variable to include in the plot.
#' @param xlab The x-axis label.
#' @param yvar The grouping variable to include in the plot.
#' @param ylab The y-axis label.
#' @param colors A named vector of colours to assign to each level of the `xvar` variable in format of `description == color code`. The order of the vector will be used in the legend.
#' @param label_width The number of characters before words wrap. Used in the y-axis and legend.
#' @param pct_cut Categories with a pct > than this value will not print the % on the plot.
#' @param legend_size The size of the text on the legend.
#' @param save A logical to determine if the plot should be saved to your working directory "plots/**.png".
#' @param bg Background color.
#' @param width Width of plot.
#' @param height Height of plot.
#' @param units Unit of plot size.
#'
#' @return A ggplot object. This means you should be able to make further adjustments to it if required.
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom scales percent
#' @import tibble
#' @import tidyr
#' @import tidyselect
#' @import ggplot2
#' @import forcats
#' @import rlang
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   "apples" = sample(
#'     forcats::as_factor(c("Good", "Neutral", "Bad")),
#'     size = 50,
#'     replace = TRUE
#'   ),
#'   "group" = sample(
#'     forcats::as_factor(c("Group 1", "Group 2", "Group 3")),
#'     size = 50,
#'     replace = TRUE
#'   )
#' )
#' cat_group_plot(
#'   df,
#'   xvar = "apples",
#'   yvar = "group",
#'   colors = c(
#'     "Bad" = "#b44218",
#'     "Neutral" = "#b4b4b4",
#'     "Good" = "#179dab"
#'   )
#' )

cat_group_plot <- function(
  .df,
  xvar,
  xlab = NULL,
  yvar,
  ylab = NULL,
  colors,
  label_width = 20,
  pct_cut = 0.06,
  legend_size = 12,
  save = FALSE,
  bg = "transparent",
  width = 15.89,
  height = 10,
  units = "cm"
) {
  plot_data <- .df %>%
    dplyr::select(.data[[xvar]], .data[[yvar]]) %>%
    tidyr::pivot_longer(!c(.data[[yvar]]))

  counts <- plot_data %>%
    tidyr::drop_na() %>%
    dplyr::count(.data[[yvar]]) %>%
    tibble::deframe()

  plot_data <- plot_data %>%
    dplyr::group_by(.data[[yvar]], name, value) %>%
    dplyr::summarise(n = n()) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(pct = n / sum(n))

  plot <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[yvar]], y = pct, fill = value)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = colors, limits = names(colors), ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_discrete(
      labels = rlang::as_function(
        ~ stringr::str_c(
          stringr::str_wrap(.x, label_width),
          "\nn = ",
          counts[.x]
        )
      ),
      position = "top"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "black", size = 12),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(colour = "black"),
      axis.title.y = ggplot2::element_text(size = 12),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = legend_size),
      plot.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(
          pct >= pct_cut,
          paste0(format(round(100 * pct, 1), nsmall = 1), "%"),
          NA
        ),
        vjust = ifelse(pct >= 0.10, "centre", "centre"),
      ),
      colour = "white",
      fontface = "bold",
      check_overlap = T,
      position = ggplot2::position_stack(vjust = 0.5)
    )

  if (save == TRUE) {
    f_name <- paste0("plots/", xvar, "_", yvar, ".png")
    ggplot2::ggsave(
      filename = f_name,
      plot = plot,
      bg = bg,
      width = width,
      height = height,
      units = units
    )
    message(paste0(
      "Plot saved as '",
      f_name,
      "', with dimensions: ",
      width,
      ":",
      height,
      ", background: '",
      bg,
      "'."
    ))
  } else if (save == FALSE) {
    message("Plot not saved")
  }

  return(plot)
}
