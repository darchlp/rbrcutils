box_group_plot <- function(df,
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
                           height = 10) {
  
  dat <- df %>%
    select(any_of(c(xvar,yvar)))
  
  if (is.numeric(df[[xvar]]) == FALSE) {
    dat <- dat %>%
      mutate(
        "{xvar}" := as.numeric(.data[[xvar]])
      )
  }
  
  if (dropna == TRUE) {
    dat <- dat %>%
      drop_na()
  } 
  
  counts <- dat %>% 
    count(.data[[yvar]]) %>%
    deframe()
  
  plot <- dat %>%
    ggplot(aes(x = .data[[xvar]], y = fct_rev(.data[[yvar]]), group = .data[[yvar]]))
  
  if (outliers == FALSE) {
    plot <- plot +
      geom_boxplot(alpha = 0)
  } else if (outliers == TRUE) {
    plot <- plot +
      geom_boxplot()
  }
  plot <- plot +
    stat_summary(fun = mean, geom = "point", aes(color = "Group mean"), shape = 18, size = 3, show.legend = TRUE)
  
  if (is.null(labels)) {
    plot <- plot +
      scale_x_continuous(limits = c(xmin,xmax))
    
  } else if (!is.null(labels) & sec.axis == TRUE) {
    plot <- plot + 
      scale_x_continuous(limits = c(xmin,xmax),
                         breaks = as.numeric(names(labels)),
                         minor_breaks = NULL,
                         labels = str_wrap(labels, label_width),
                         # expand = expansion(mult = c(0, 0.12)),
                         sec.axis = dup_axis(
                           name = "Numeric scores",
                           labels = names(labels)
                         )
      )
  } else if (!is.null(labels) & sec.axis == FALSE) {
    plot <- plot + 
      scale_x_continuous(limits = c(xmin,xmax),
                         breaks = as.numeric(names(labels)),
                         minor_breaks = NULL,
                         labels = str_wrap(labels, label_width),
                         # expand = expansion(mult = c(0, 0.12)),
      )
  }
  
  # Overall mean line
  plot <- plot + 
    geom_vline(
      aes(xintercept = mean(as.numeric(df[[xvar]]), na.rm = TRUE),
          color = "Overall mean", linetype = "Overall mean"),
      show.legend = TRUE
    ) +
    geom_vline(
      aes(xintercept = median(as.numeric(df[[xvar]]), na.rm = TRUE),
          color = "Overall median", linetype = "Overall median"),
      show.legend = TRUE
    ) +
    scale_color_manual(name = NULL,
                       values = c(
                         "Group mean" = "#9b7d5e",
                         "Overall mean" = "#179DAB",
                         "Overall median" = "#179DAB"
                       )) +
    scale_linetype_manual(name = NULL,
                          values = c(
                            "Overall mean" = "dashed",
                            "Overall median" = "solid"
                          )) +
    guides(
      color = guide_legend(override.aes = list(
        shape = c(18, NA, NA),  # point, line, line
        linetype = c("blank", "dashed", "solid"),
        size = c(3, 2, 2)
      )),
      linetype = "none"
    ) + 
    labs(x = xvar_name,
         y = yvar_name) +
    scale_y_discrete(labels = as_function(~ str_c(.x, "\nn = ", counts[.x]))) + # add in the n counts
    theme_minimal() +
    theme(text = element_text(color="black"),
          axis.text = element_text(color="black"),
          # axis.text.x = element_text(hjust = 0.6),
          plot.background = element_rect(fill='transparent', color=NA),
          legend.position = "bottom") 
  
  if (coord_flip == TRUE) {
    plot <- plot +
      coord_flip()
  }
  
  if (save == TRUE) {
    f_name <- paste0("plots/summary_box_workplace/",xvar,"_",yvar,".png")
    ggsave(filename = f_name, 
           plot = plot,
           bg=bg,
           width = width,
           height = height,
           units = "cm")
    print(paste0("Plot saved as '", f_name,"', with dimensions: ",
                 width, ":",height, "cm, background: '", bg, "'."))
  } else if (save == FALSE) {
    print("Plot not saved")
  }
  
  #list(plot = plot)
  return(plot)
  
  
}

# box_group_plot(
#   scored,
#   "v_abuse_num_rs",
#   "Frequency of verbal abuse",
#   "workplace_collapsed",
#   "Workplace",
#   labels = c(
#     "1" = "Never",
#     "2" = "Less than once per week",
#     "3" = "Once per week",
#     "4" = "2-3 times per week",
#     "5" = "4-6 times per week",
#     "6" = "Every day",
#     "7" = "More than once per day"
#   ),
#   label_width = 10,
#   save = F
# )