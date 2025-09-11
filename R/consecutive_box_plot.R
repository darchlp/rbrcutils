consecutive_box_plot <- function(df,
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
                            height = 10) {
  
  plot_data <- df %>%
    select(record_id, all_of(var)) %>%
    mutate(
      across(
        all_of(names(var)), ~ as.numeric(.x)
      )
    ) %>%
    pivot_longer(!c("record_id")) %>%
    mutate(name = fct_relevel(name, names(var)))
  
  counts <- plot_data %>%
    drop_na() %>%
    count(name) %>%
    deframe()
  
  if (ordered_mean == TRUE) {
    plot <- plot_data %>%
      ggplot(aes(x = value, y = fct_reorder(name,value,.fun = "mean")))
  } else if (ordered_mean == FALSE) {
    plot <- plot_data %>%
      ggplot(aes(x = value, y = name))
  }
  
  plot <- plot +
    geom_boxplot() +
    xlab(xlab) +
    ylab(ylab) + 
    scale_y_discrete(labels = as_function(~ str_c(str_wrap(.x,label_width), "\nn = ", counts[.x]))) + # add in the n counts
    
    theme_minimal() +
    theme(text = element_text(color="black"),
          axis.text = element_text(color="black"),
          # axis.text.x = element_text(hjust = 0.6),
          plot.background = element_rect(fill='transparent', color=NA),
          legend.position = "bottom") 
  
  if (is.null(labels) & reverse == FALSE) {
    plot <- plot +
      scale_x_continuous(limits = c(xmin,xmax))
  } else if (is.null(labels) & reverse == TRUE) {
    plot <- plot +
      scale_x_continuous(limits = c(xmin,xmax),
                         transform = "reverse")
  } else if (!is.null(labels) & reverse == FALSE & sec.axis == FALSE) {
    plot <- plot +
      scale_x_continuous(limits = c(xmin,xmax),
                         breaks = as.numeric(names(labels)),
                         minor_breaks = NULL,
                         labels = str_wrap(labels, label_width))
  } else if (!is.null(labels) & reverse == FALSE & sec.axis == TRUE) {
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
  } else if (!is.null(labels) & reverse == TRUE) {
    plot <- plot + 
      scale_x_continuous(limits = c(xmin,xmax),
                         breaks = as.numeric(names(labels)),
                         minor_breaks = NULL,
                         labels = str_wrap(labels, label_width),
                         # expand = expansion(mult = c(0, 0.12)),
                         sec.axis = dup_axis(
                           name = "Numeric scores",
                           labels = names(labels)
                         ),
                         transform = "reverse"
      )
  }
  
  if (mean == TRUE & global_mean == FALSE) {
    plot <- plot +
      stat_summary(fun = "mean", geom = "point", aes(color = "Domain mean"), shape = 18, size = 3, show.legend = TRUE) +
      guides(
        color = guide_legend(override.aes = list(
          shape = c(18),  
          size = c(3)
        )),
        linetype = "none"
      ) +
      scale_color_manual(name = NULL,
                         values = c(
                           "Domain mean" = "#9b7d5e"
                         )) 
  }
  
  if (global_mean == TRUE & mean == FALSE) {
  # Overall mean line
  plot <- plot + 
    geom_vline(
      aes(xintercept = mean(as.numeric(.data[["value"]]), na.rm = TRUE),
          color = "Overall mean", linetype = "Overall mean"),
      show.legend = TRUE
    ) +
    geom_vline(
      aes(xintercept = median(as.numeric(.data[["value"]]), na.rm = TRUE),
          color = "Overall median", linetype = "Overall median"),
      show.legend = TRUE
    ) +
    scale_color_manual(name = NULL,
                       values = c(
                         # "Domain mean" = "#9b7d5e",
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
        shape = c(
          # 18,
                  NA, NA),  # point, line, line
        linetype = c(
          # "blank",
          "dashed", "solid"),
        size = c(
          # 3,
                 2, 2)
      )),
      linetype = "none"
    )
  }
  
  if (global_mean == TRUE & mean == TRUE) {
    # Overall mean line
    plot <- plot + 
      stat_summary(fun = "mean", geom = "point", aes(color = "Domain mean"), shape = 18, size = 3, show.legend = TRUE) +
      geom_vline(
        aes(xintercept = mean(as.numeric(.data[["value"]]), na.rm = TRUE),
            color = "Overall mean", linetype = "Overall mean"),
        show.legend = TRUE
      ) +
      geom_vline(
        aes(xintercept = median(as.numeric(.data[["value"]]), na.rm = TRUE),
            color = "Overall median", linetype = "Overall median"),
        show.legend = TRUE
      ) +
      scale_color_manual(name = NULL,
                         values = c(
                           "Domain mean" = "#9b7d5e",
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
          shape = c(18,NA, NA),  # point, line, line
          linetype = c("blank","dashed", "solid"),
          size = c(3,2, 2)
        )),
        linetype = "none"
      )
  }
  
  if (save == TRUE) {
    f_name <- paste0("plots/summary_box_global/",var[1],"_distribution.png")
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

# consecutive_box_plot(c("Coworker relationship" = "coworker_relationship",
#                   "Coworker conflict" =  "coworker_conflict"),
#                 labels = c("1" = "Strongly diagree",
#                            "2" = "Disagree",
#                            "3" = "Neutral",
#                            "4" = "Agree",
#                            "5" = "Strongly agree"),
#                 reverse = T,
#                 ylab = "Coworker relationship", 
#                 mean = T,
#                 global_mean = T)
