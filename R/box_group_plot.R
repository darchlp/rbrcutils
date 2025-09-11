#' Plot a numerical (or categorical which can be converted to numerical) by a grouping variable.
#'
#' @param .df A data.frame or tibble that contains the categorical variables in `var`.
#' @param xvar A numeric variable (or factor which can be converted to numeric) to plot on the x-axis.
#' @param xvar_name The x-axis name.
#' @param yvar A categorical variable to plot on the y-axis.
#' @param yvar_name The y-axis name.
#' @param dropna Plot missing grouping levels (i.e., NA groupings).
#' @param outliers Plot points representing outliers.
#' @param reverse Reverse the x-axis.
#' @param xmin Overwrite the minimum x-axis value if the value does not appear in the dataset.
#' @param xmax Overwrite the maximum x-axis value if the value does not appear in the dataset.
#' @param labels A named vector to provide the x-axis labels with human readable levels.
#' @param sec.axis A secondary axis to plot the numeric scores.
#' @param label_width The number of characters before words wrap. Used in the x- and y-axis labels.
#' @param coord_flip Have the boxplots vertical instead of horizontal.
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
#' @import tidyr
#' @import tidyselect
#' @import ggplot2
#' @import forcats
#' @import rlang
#' @import stats
#'
#' @examples
#' df <- data.frame(
#'   "record_id" = 1:50,
#'   "number" = sample(1:5, size = 50, replace = T),
#'   "group" = sample(as_factor(c("Group 1", "Group 2", "Group 3")), size = 50, replace = T)
#' )
#' 
#' box_group_plot(
#'   df,
#'   xvar = "number",
#'   xvar_name = "Numeric",
#'   yvar = "group",
#'   yvar_name = "Grouping",
#'   labels = c(
#'     "1" = "Strongly disagree",
#'     "2" = "Disagree",
#'     "3" = "Neutral",
#'     "4" = "Agree",
#'     "5" = "Strongly agree"
#'   )
#' )
#' 

box_group_plot <- function(.df,
                           xvar,
                           xvar_name = xvar,
                           yvar,
                           yvar_name = yvar,
                           dropna = TRUE,
                           outliers = TRUE,
                           reverse = FALSE,
                           xmin = NA,
                           xmax = NA,
                           labels = NULL,
                           sec.axis = TRUE,
                           label_width = 20,
                           coord_flip = FALSE,
                           save = FALSE,
                           bg = "transparent",
                           width = 15.89,
                           height = 10,
                          units = "cm") {
  
  dat <- .df %>%
    dplyr::select(tidyselect::any_of(c(xvar,yvar)))
  
  if (is.numeric(.df[[xvar]]) == FALSE) {
    dat <- dat %>%
      dplyr::mutate(
        "{xvar}" := as.numeric(.data[[xvar]])
      )
  }
  
  if (dropna == TRUE) {
    dat <- dat %>%
      tidyr::drop_na()
  } 
  
  counts <- dat %>% 
    dplyr::count(.data[[yvar]]) %>%
    tibble::deframe()
  
  plot <- dat %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[xvar]], y = forcats::fct_rev(.data[[yvar]]), group = .data[[yvar]]))
  
  if (outliers == FALSE) {
    plot <- plot +
      ggplot2::geom_boxplot(alpha = 0)
  } else if (outliers == TRUE) {
    plot <- plot +
      ggplot2::geom_boxplot()
  }
  plot <- plot +
    ggplot2::stat_summary(fun = mean, geom = "point", ggplot2::aes(color = "Group mean"), shape = 18, size = 3, show.legend = TRUE)
  
  if (is.null(labels)) {
    plot <- plot +
      ggplot2::scale_x_continuous(limits = c(xmin,xmax))
    
  } else if (!is.null(labels) & sec.axis == TRUE) {
    plot <- plot + 
      ggplot2::scale_x_continuous(limits = c(xmin,xmax),
                         breaks = as.numeric(names(labels)),
                         minor_breaks = NULL,
                         labels = stringr::str_wrap(labels, label_width),
                         sec.axis = ggplot2::dup_axis(
                           name = "Numeric scores",
                           labels = names(labels)
                         )
      )
  } else if (!is.null(labels) & sec.axis == FALSE) {
    plot <- plot + 
      ggplot2::scale_x_continuous(limits = c(xmin,xmax),
                         breaks = as.numeric(names(labels)),
                         minor_breaks = NULL,
                         labels = stringr::str_wrap(labels, label_width),
      )
  }
  
  # Overall mean line
  plot <- plot + 
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = mean(as.numeric(.df[[xvar]]), na.rm = TRUE),
          color = "Overall mean", linetype = "Overall mean"),
      show.legend = TRUE
    ) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = median(as.numeric(.df[[xvar]]), na.rm = TRUE),
          color = "Overall median", linetype = "Overall median"),
      show.legend = TRUE
    ) +
    ggplot2::scale_color_manual(name = NULL,
                       values = c(
                         "Group mean" = "#9b7d5e",
                         "Overall mean" = "#179DAB",
                         "Overall median" = "#179DAB"
                       )) +
    ggplot2::scale_linetype_manual(name = NULL,
                          values = c(
                            "Overall mean" = "dashed",
                            "Overall median" = "solid"
                          )) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(
        shape = c(18, NA, NA),  # point, line, line
        linetype = c("blank", "dashed", "solid"),
        size = c(3, 2, 2)
      )),
      linetype = "none"
    ) + 
    ggplot2::labs(x = xvar_name,
         y = yvar_name) +
    ggplot2::scale_y_discrete(labels = rlang::as_function(~ stringr::str_c(.x, "\nn = ", counts[.x]))) + # add in the n counts
    ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(color="black"),
          axis.text = ggplot2::element_text(color="black"),
          plot.background = ggplot2::element_rect(fill='transparent', color=NA),
          legend.position = "bottom") 
  
  if (coord_flip == TRUE) {
    plot <- plot +
      ggplot2::coord_flip()
  }
  
  if (save == TRUE) {
    f_name <- paste0("plots/summary_box_workplace/",xvar,"_",yvar,".png")
    ggplot2::ggsave(filename = f_name, 
           plot = plot,
           bg=bg,
           width = width,
           height = height,
           units = units)
    print(paste0("Plot saved as '", f_name,"', with dimensions: ",
                 width, ":",height, "cm, background: '", bg, "'."))
  } else if (save == FALSE) {
    print("Plot not saved")
  }
  
  return(plot)
  
}