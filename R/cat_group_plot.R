#' Plot a categorical variable over a grouping variable.
#'
#' @param .df A data.frame or tibble that contains a categorical and grouping variable.
#' @param xvar The categorical variable to include in the plot.
#' @param xlab The x-axis label.
#' @param yvar The grouping variable to include in the plot.
#' @param ylab The y-axis label.
#' @param horizontal To flip the plot to be horizontal.
#' @param colors A named vector of colours to assign to each level of the `xvar` variable in format of `description == color code`. The order of the vector will be used in the legend.
#' @param label_width The number of characters before y-axis labels wrap. 
#' @param wrap_length The number of characters before the legend labels wrap.
#' @param pct_cut Categories with a pct > than this value will not print the % on the plot.
#' @param pct_label To show the % sign or not.
#' @param xaxis_size The size of the text on the x-axis.
#' @param yaxis_size The size of the text on the y-axis.
#' @param round_digits Number of digits to round to.
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
#' @importFrom stringr str_wrap
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
  horizontal = F,
  colors,
  label_width = 20,
  pct_cut = 0.06,
  text_size = NULL,
  pct_label = T,
  xaxis_size = 9,
  yaxis_size = 9,
  wrap_length = 14,
  round_digits = 1,
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
    ggplot2::scale_fill_manual(values = colors, limits = names(colors), labels = stringr::str_wrap(names(colors),wrap_length)) +                    
    ggplot2::scale_y_continuous(labels = scales::percent)

  if (horizontal == TRUE) {
   plot <- plot +
     ggplot2::scale_x_discrete(
      labels = rlang::as_function(
        ~ stringr::str_c(
          stringr::str_wrap(.x, label_width),
          "\nn = ",
          counts[.x]
        )
      ),
      position = "bottom"
    )
  } else if (horizontal == FALSE) {
    plot <- plot +
      ggplot2::scale_x_discrete(
      labels = rlang::as_function(
        ~ stringr::str_c(
          stringr::str_wrap(.x, label_width),
          "\nn = ",
          counts[.x]
        )
      ),
      position = "top"
    )  
  }
    plot <- plot +
      ggplot2::theme_minimal() +
      ggplot2::theme(
      text = ggplot2::element_text(colour = "black", size = 12),
      axis.text.x = ggplot2::element_text(size = xaxis_size),
      axis.text.y = ggplot2::element_text(size = yaxis_size),
      axis.text = ggplot2::element_text(colour = "black"),
      axis.title.y = ggplot2::element_text(size = 12),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = legend_size),
      plot.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = ylab, y = xlab) + # These need to be the other way 
    ggplot2::coord_cartesian(
      clip = "off"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(
          pct >= pct_cut,
          paste0(format(round(100 * pct, round_digits), nsmall = round_digits), ifelse(pct_label,"%","")),
          NA
        ),
        vjust = ifelse(pct >= 0.10, "centre", "centre")
      ),
      colour = "white",
      size = text_size,
      fontface = "bold",
      check_overlap = T,
      position = ggplot2::position_stack(vjust = 0.5)
    ) +
    if (horizontal) coord_flip() 

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
