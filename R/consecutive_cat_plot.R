#' Plot a series of categorical variables.
#'
#' @param .df A data.frame or tibble that contains the categorical variables in `var`.
#' @param var A vector of categorical variables to include in the plot. Can be a named list to automatically rename the variables in the plot.
#' @param colors A named vector of colours to assign to each level of the `var` variable in format of `description == color code`. The order of the vector will be used in the legend.
#' @param y_lab A logical indicating if the y-axis labels should be displayed.
#' @param print_data A helper option to print some raw data to the console. Can be helpful when debugging, or adding data to reports.
#' @param rev_fill Used to reverse the color of the bars in the plot.
#' @param text_size The size of the text in the labels.
#' @param pct_label To show the % sign or not.
#' @param xaxis_size The size of the text on the x-axis.
#' @param yaxis_size The size of the text on the y-axis.
#' @param round_digits Number of digits to round to.
#' @param legend_size The size of the text on the legend.
#' @param label_width The number of characters before words wrap. Used in the y-axis and legend.
#' @param pct_cut Categories with a pct > than this value will not print the % on the plot.
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
#' @import tidyr
#' @import tidyselect
#' @importFrom scales percent
#' @import tibble
#' @import ggplot2
#' @import forcats
#' @import rlang
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   "apples" = sample(forcats::as_factor(c("Good", "Neutral", "Bad")), size = 50, replace = TRUE),
#'   "bananas" = sample(forcats::as_factor(c("Good", "Neutral", "Bad")), size = 50, replace = TRUE),
#'   "pears" = sample(forcats::as_factor(c("Good", "Neutral", "Bad")), size = 50, replace = TRUE)
#' )
#'
#' consecutive_cat_plot(
#'   df,
#'   c("Apples" = "apples", "Bananas" = "bananas", "Pears" = "pears"),
#'   colors = c(
#'     "Bad" = "#b44218",
#'     "Neutral" = "#b4b4b4",
#'     "Good" = "#179dab"
#'   )
#' )

consecutive_cat_plot <- function(
  .df,
  var,
  colors,
  y_lab = TRUE,
  print_data = FALSE,
  rev_fill = FALSE,
  text_size = NULL,
  pct_label = T,
  xaxis_size = 9,
  yaxis_size = 9,
  round_digits = 1,
  legend_size = 12,
  label_width = 14,
  pct_cut = 0.15,
  save = FALSE,
  bg = "transparent",
  width = 15.89,
  height = 16,
  units = "cm"
) {
  dat <- .df %>%
    dplyr::select(tidyselect::all_of(var)) %>%
    tidyr::pivot_longer(dplyr::everything())

  counts <- dat %>%
    tidyr::drop_na() %>%
    dplyr::count(name) %>%
    tibble::deframe()

  plot_data <- dat %>%
    dplyr::group_by(name, value) %>%
    dplyr::summarise(n = n()) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(pct = n / sum(n)) %>%
    dplyr::ungroup()

  if (print_data == T) {
    print(plot_data, n = nrow(plot_data))
  }

  if (rev_fill == TRUE) {
    plot <- plot_data %>%
      ggplot2::ggplot(ggplot2::aes(
        x = pct,
        y = forcats::fct_reorder2(name, value, dplyr::desc(pct)),
        fill = forcats::fct_rev(value)
      ))
  } else if (rev_fill == FALSE) {
    plot <- plot_data %>%
      ggplot2::ggplot(ggplot2::aes(
        x = pct,
        y = forcats::fct_reorder2(name, value, dplyr::desc(pct)),
        fill = value
      ))
  } else if (rev_fill == "as_is") {
    plot <- plot_data %>%
      ggplot2::ggplot(ggplot2::aes(
        x = pct,
        y = forcats::fct_rev(name),
        fill = forcats::fct_rev(value)
      ))
  }
  plot <- plot +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      values = colors,
      limits = names(colors),
      breaks = names(colors),
      labels = stringr::str_wrap(names(colors), label_width)
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent)

  if (y_lab == TRUE) {
    plot <- plot +
      ggplot2::scale_y_discrete(
        # add in the n counts
        labels = rlang::as_function(
          ~ stringr::str_c(
            stringr::str_wrap(.x, label_width),
            "\nn = ",
            counts[.x]
          )
        )
      )
  } else if (y_lab == FALSE) { 
    plot <- plot +
      ggplot2::scale_y_discrete( # I almost always want to show the n count per row
        # add in the n counts
        labels = rlang::as_function(
          ~ stringr::str_c(
            # stringr::str_wrap(.x, label_width),
            "n = ",
            counts[.x]
          )
        )
      )
  }
  plot <- plot +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "black", size = 16),
      axis.text.x = ggplot2::element_text(size = xaxis_size),
      axis.text.y = ggplot2::element_text(size = yaxis_size),
      axis.text = ggplot2::element_text(colour = "black"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = legend_size),
      plot.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      plot.caption = ggplot2::element_text(colour = "black", size = 7),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.spacing = unit(0, "npc"),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank()
    ) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::coord_cartesian(clip = "off") +

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
    )

  if (save == TRUE) {
    f_name <- paste0("plots/", var[[1]], "_.png")
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

