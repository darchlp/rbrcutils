cat_group_plot <- function(df,
                           xvar,
                           xlab = NULL,
                           yvar,
                           ylab = NULL,
                           colors,
                           label_width = 20,
                           lbl_cutoff = 0.06,
                           wrap_length = 14,
                           legend.text = 12,
                           save = FALSE,
                           bg = "transparent",
                           width = 15.89,
                           height = 10,
                           units = "cm") {
  
  plot_data <- df %>%
    select(record_id, 
           .data[[xvar]],
           .data[[yvar]]
    ) %>%
    pivot_longer(!c(record_id,.data[[yvar]]))
  
  
  counts <- plot_data %>%
    drop_na() %>%
    count(.data[[yvar]]) %>%
    deframe()
  
  # print(counts)
  
  plot_data <- plot_data %>%
    group_by(.data[[yvar]],name,value) %>%
    summarise(n = n()) %>%
    drop_na() %>%
    mutate(pct = n/sum(n)) 
  
  plot <- plot_data %>%
    ggplot(aes(x = .data[[yvar]], y = pct, fill = value)) + 
    geom_col() + 
    
    scale_fill_manual(values = colors,
                      limits = names(colors),
    ) +
    scale_y_continuous(labels = scales::percent) + 
    scale_x_discrete(
      labels = as_function(~ str_c(str_wrap(.x,label_width), "\nn = ", counts[.x])),
      position = "top"
    ) + 
    
    
    
    theme_minimal() + 
    theme(text = element_text(colour = "black",
                              size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.text = element_text(colour = "black"),
          axis.title.y = element_text(size = 12),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = legend.text),
          plot.background = element_rect(fill='transparent', color=NA),
          legend.background = element_blank(),
          legend.box.background = element_blank()
    ) +
    labs(x = xlab,
         y = ylab) + 
    coord_cartesian(clip = "off") + 
    geom_text(
      aes(
        label = 
          ifelse(pct >= lbl_cutoff,
                 paste0(format(round(100*pct, 1), nsmall = 1), "%"),
                 NA),
        vjust = ifelse(pct >= 0.10, "centre", "centre"),
      ),
      colour = "white",
      fontface = "bold",
      #size = 3,
      check_overlap = T,
      position = position_stack(vjust = 0.5)
    ) 
  
  if (save == TRUE) {
    f_name <- paste0("plots/",xvar,"_", yvar,".png")
    ggsave(filename = f_name, 
           plot = plot,
           bg=bg,
           width = width,
           height = height,
           units = units)
    print(paste0("Plot saved as '", f_name,"', with dimensions: ",
                 width, ":",height, ", background: '", bg, "'."))
  } else if (save == FALSE) {
    print("Plot not saved")
  }
  
  #list(plot = plot)
  return(plot)
  
}