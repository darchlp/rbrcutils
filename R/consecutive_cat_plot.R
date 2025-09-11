consecutive_cat_plot <- function(data,
                                 var,
                                 colors, 
                                 print_data = FALSE,
                                 rev_fill = FALSE,
                                 xaxis_size = 9,
                                 yaxis_size = 9,
                                 legend_size = 12,
                                 label_width = 14,
                                 pct_cut = 0.15,
                                 save = FALSE,
                                 bg = "transparent",
                                 width = 15.89,
                                 height = 16,
                                 units = "cm") {
  dat <- data %>%
    select(record_id, all_of(var)) %>%
    pivot_longer(!c(record_id)) 
  
  counts <- dat %>%
    drop_na() %>%
    count(name) %>%
    deframe()
  
  plot_data <- dat %>%
    group_by(name,value) %>%
    summarise(n = n()) %>%
    drop_na() %>%
    mutate(pct = n/sum(n)) %>%
    ungroup() 
  
  if (print_data == T) {
    print(plot_data, n = nrow(plot_data))
  }
  
  if (rev_fill == TRUE) {
    plot <- plot_data %>%
      ggplot(aes(x = pct, y = fct_reorder2(name,value,desc(pct)), fill = fct_rev(value))) 
  } else if (rev_fill == FALSE) {
    plot <- plot_data %>%
      ggplot(aes(x = pct, y = fct_reorder2(name,value,desc(pct)), fill = value))  
  } else if (rev_fill == "as_is") {
    plot <- plot_data %>%
      ggplot(aes(x = pct, y = fct_rev(name), fill = fct_rev(value)))  
  }
  plot <- plot +
    geom_col() +
    scale_fill_manual(values = colors,
                      limits = names(colors),
                      breaks = names(colors),
                      labels = str_wrap(names(colors),label_width)
    ) +
    scale_x_continuous(labels = scales::percent) + 
    scale_y_discrete(labels = as_function(~ str_c(str_wrap(.x,label_width), "\nn = ", counts[.x]))) + # add in the n counts
    theme_minimal() +
    theme(text = element_text(colour = "black",
                              size = 16),
          axis.text.x = element_text(size = xaxis_size),
          axis.text.y = element_text(size = yaxis_size),
          axis.text = element_text(colour = "black"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = legend_size),
          plot.background = element_rect(fill='transparent', color=NA),
          plot.caption = element_text(colour = "black",size = 7),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.spacing = unit(0,"npc"),
          legend.background = element_blank(),
          legend.box.background = element_blank()
          
    ) +
    xlab(NULL) +
    ylab(NULL) + 
    coord_cartesian(clip = "off") +
    geom_text(
      aes(
        label = 
          ifelse(pct >= pct_cut,
                 paste0(format(round(100*pct, 1), nsmall = 1), "%"),
                 NA),
        vjust = ifelse(pct >= 0.10, "centre", "centre"),
      ),
      colour = "white",
      fontface = "bold",
      check_overlap = T,
      position = position_stack(vjust = 0.5)
    )
  
  
  
  if (save == TRUE) {
    f_name <- paste0("plots/",var[[1]],"_.png")
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

# consecutive_cat_plot(
#   scored,
#   c(
#     "Support for new staff" = "supportnew_collapsed",
#     "Access to mental health support" = "mentalsupport_collapsed",
#     "Preventing physical violence" = "violence_collapsed",
#     "Preventing verbal abuse" = "abuse_collapsed",
#     "Exposure to infectious patients" = "exposure_collapsed",
#     "Minimising radiation exposure" = "radiation_collapsed",
#     "Use of telehealth" = "telehealth_collapsed",
#     "Ability to deploy more staff" = "capacity_collapsed",
#     "Isolation of vulnerable patients" = "isolation_collapsed",
#     "Communication of policies and procedures" = "communication_collapsed",
#     "Safe exposure to chemicals" = "chemicals_collapsed",
#     "Cleaning practices" = "cleaning_collapsed",
#     "Access to manual handling tools" = "access_equip_collapsed",
#     "Handover processes" = "handover_collapsed",
#     "Debriefing after incident" = "debriefing_collapsed",
#     "Skill mix" = "skills_mix_collapsed",
#     "Staffing levels" = "staffing_levels_collapsed"
#   ),
#   colors = c(
#     "Satisfactory" = "#179dab",
#     "Unsatisfactory" = "#b4b4b4",
#     "No policy" = "#b44218"
#   ),
#   label_width = 20,
#   save = F
# )