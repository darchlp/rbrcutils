#' Plot a series of numerical variables in the form of a box plot.
#'
#' @param .df A data.frame or tibble that contains the categorical variables in `var`.
#' @param var A vector of numerical variables to include in the plot. Can be a named list to automatically rename the variables in the plot.
#' @param labels A named vector to provide the x-axis labels with human readable levels.
#' @param label_width The number of characters before words wrap. Used in the x- and y-axis labels.
#' @param xlab The x-axis name.
#' @param ylab The y-axis name.
#' @param xmin Overwrite the minimum x-axis value if the value does not appear in the dataset.
#' @param xmax Overwrite the maximum x-axis value if the value does not appear in the dataset.
#' @param sec.axis A secondary axis to plot the numeric scores.
#' @param reverse Reverse the x-axis.
#' @param ordered_mean Order the `var` on the y-axis by mean
#' @param mean Display the mean of each variable
#' @param global_mean Display the global mean and median of all variables. Useful when displaying the results of all variables in a measure.
#' @param save A logical to determine if the plot should be saved to your working directory "plots/**.png".
#' @param bg Background color.
#' @param width Width of plot.
#' @param height Height of plot.
#' @param units Unit of plot size.
#'
#' @return A ggplot object. This means you should be able to make further adjustments to it if required.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats median
#' @import tidyr
#' @import tidyselect
#' @import tibble
#' @import ggplot2
#' @import forcats
#' @import rlang
#' 
#' @export
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   "number1" = sample(1:5, size = 50, replace = TRUE),
#'   "number2" = sample(1:5, size = 50, replace = TRUE),
#'   "number3" = sample(1:5, size = 50, replace = TRUE)
#' )
#' 
#' consecutive_box_plot(
#'   df,
#'   var = c(
#'     "Number 1" = "number1",
#'     "Number 2" = "number2",
#'     "Number 3" = "number3"
#'   ),
#'   labels = c(
#'     "1" = "Strongly diagree",
#'     "2" = "Disagree",
#'     "3" = "Neutral",
#'     "4" = "Agree",
#'     "5" = "Strongly agree"
#'   ),
#'   ordered_mean = TRUE
#' )
#' 
#' 

consecutive_box_plot <- function(
  .df,
  var,
  labels,
  label_width = 10,
  xlab = NULL,
  ylab = NULL,
  xmin = NA,
  xmax = NA,
  sec.axis = TRUE,
  reverse = FALSE,
  ordered_mean = FALSE,
  mean = TRUE,
  global_mean = TRUE,
  save = FALSE,
  bg = "transparent",
  width = 15.89,
  height = 10,
  units = "cm"
) {
  plot_data <- .df %>%
    dplyr::select(dplyr::all_of(var)) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(names(var)),
        ~ as.numeric(.x)
      )
    ) %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::mutate(name = forcats::fct_relevel(name, names(var)))

  counts <- plot_data %>%
    tidyr::drop_na() %>%
    dplyr::count(name) %>%
    tibble::deframe()

  if (ordered_mean == TRUE) {
    plot <- plot_data %>%
      ggplot2::ggplot(ggplot2::aes(
        x = value,
        y = forcats::fct_reorder(name, value, .fun = "mean")
      ))
  } else if (ordered_mean == FALSE) {
    plot <- plot_data %>%
      ggplot2::ggplot(ggplot2::aes(x = value, y = name))
  }

  plot <- plot +
    ggplot2::geom_boxplot() +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::scale_y_discrete(
      labels = rlang::as_function(
        ~ stringr::str_c(
          stringr::str_wrap(.x, label_width),
          "\nn = ",
          counts[.x]
        )
      )
    ) + # add in the n counts
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(color = "black"),
      axis.text = ggplot2::element_text(color = "black"),
      plot.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      legend.position = "bottom"
    )

  if (is.null(labels) & reverse == FALSE) {
    plot <- plot +
      ggplot2::scale_x_continuous(limits = c(xmin, xmax))
  } else if (is.null(labels) & reverse == TRUE) {
    plot <- plot +
      ggplot2::scale_x_continuous(limits = c(xmin, xmax), transform = "reverse")
  } else if (!is.null(labels) & reverse == FALSE & sec.axis == FALSE) {
    plot <- plot +
      ggplot2::scale_x_continuous(
        limits = c(xmin, xmax),
        breaks = as.numeric(names(labels)),
        minor_breaks = NULL,
        labels = stringr::str_wrap(labels, label_width)
      )
  } else if (!is.null(labels) & reverse == FALSE & sec.axis == TRUE) {
    plot <- plot +
      ggplot2::scale_x_continuous(
        limits = c(xmin, xmax),
        breaks = as.numeric(names(labels)),
        minor_breaks = NULL,
        labels = stringr::str_wrap(labels, label_width),
        # expand = expansion(mult = c(0, 0.12)),
        sec.axis = ggplot2::dup_axis(
          name = "Numeric scores",
          labels = names(labels)
        )
      )
  } else if (!is.null(labels) & reverse == TRUE) {
    plot <- plot +
      ggplot2::scale_x_continuous(
        limits = c(xmin, xmax),
        breaks = as.numeric(names(labels)),
        minor_breaks = NULL,
        labels = stringr::str_wrap(labels, label_width),
        # expand = expansion(mult = c(0, 0.12)),
        sec.axis = ggplot2::dup_axis(
          name = "Numeric scores",
          labels = names(labels)
        ),
        transform = "reverse"
      )
  }

  if (mean == TRUE & global_mean == FALSE) {
    plot <- plot +
      ggplot2::stat_summary(
        fun = "mean",
        geom = "point",
        aes(color = "Domain mean"),
        shape = 18,
        size = 3,
        show.legend = TRUE
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(18),
            size = c(3)
          )
        ),
        linetype = "none"
      ) +
      ggplot2::scale_color_manual(
        name = NULL,
        values = c(
          "Domain mean" = "#9b7d5e"
        )
      )
  }

  if (global_mean == TRUE & mean == FALSE) {
    # Overall mean line
    plot <- plot +
      ggplot2::geom_vline(
        ggplot2::aes(
          xintercept = mean(as.numeric(.data[["value"]]), na.rm = TRUE),
          color = "Overall mean",
          linetype = "Overall mean"
        ),
        show.legend = TRUE
      ) +
      ggplot2::geom_vline(
        ggplot2::aes(
          xintercept = stats::median(as.numeric(.data[["value"]]), na.rm = TRUE),
          color = "Overall median",
          linetype = "Overall median"
        ),
        show.legend = TRUE
      ) +
      ggplot2::scale_color_manual(
        name = NULL,
        values = c(
          "Overall mean" = "#179DAB",
          "Overall median" = "#179DAB"
        )
      ) +
      ggplot2::scale_linetype_manual(
        name = NULL,
        values = c(
          "Overall mean" = "dashed",
          "Overall median" = "solid"
        )
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(
              # 18,
              NA,
              NA
            ), # point, line, line
            linetype = c(
              # "blank",
              "dashed",
              "solid"
            ),
            size = c(
              2,
              2
            )
          )
        ),
        linetype = "none"
      )
  }

  if (global_mean == TRUE & mean == TRUE) {
    # Overall mean line
    plot <- plot +
      ggplot2::stat_summary(
        fun = "mean",
        geom = "point",
        ggplot2::aes(color = "Domain mean"),
        shape = 18,
        size = 3,
        show.legend = TRUE
      ) +
      ggplot2::geom_vline(
        ggplot2::aes(
          xintercept = mean(as.numeric(.data[["value"]]), na.rm = TRUE),
          color = "Overall mean",
          linetype = "Overall mean"
        ),
        show.legend = TRUE
      ) +
      ggplot2::geom_vline(
        aes(
          xintercept = median(as.numeric(.data[["value"]]), na.rm = TRUE),
          color = "Overall median",
          linetype = "Overall median"
        ),
        show.legend = TRUE
      ) +
      ggplot2::scale_color_manual(
        name = NULL,
        values = c(
          "Domain mean" = "#9b7d5e",
          "Overall mean" = "#179DAB",
          "Overall median" = "#179DAB"
        )
      ) +
      ggplot2::scale_linetype_manual(
        name = NULL,
        values = c(
          "Overall mean" = "dashed",
          "Overall median" = "solid"
        )
      ) +
      ggplot2::guides(
        color = ggplot2::guide_legend(
          override.aes = list(
            shape = c(18, NA, NA), # point, line, line
            linetype = c("blank", "dashed", "solid"),
            size = c(3, 2, 2)
          )
        ),
        linetype = "none"
      )
  }

  if (save == TRUE) {
    f_name <- paste0("plots/summary_box_global/", var[1], "_distribution.png")
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
      "cm, background: '",
      bg,
      "'."
    ))
  } else if (save == FALSE) {
    message("Plot not saved")
  }

  return(plot)
}
