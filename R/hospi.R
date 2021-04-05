## HOSPI
## Line plot incidence hospi hors SI
date_report <- format(max(hospi_i$date), '%d%h%Y')
hospi_line <- hospi_i %>%
    ggplot(aes(date, hospi)) +
    geom_bar(stat = "identity", fill = paletteer_d("nord::polarnight")[4]) +
    geom_line(aes(date, roll_hospi, group = 1, lty = "Moyenne mobile 7 jours"),
              colour = paletteer_d("nord::frost")[4], size = 1.1) +
    scale_x_date(date_breaks = "months",
                 labels = date_format("%b")) +
    labs(title = "Nouvelles hospitalisations au Québec",
         subtitle = paste(date_report, ":", round(tail(hospi_i$roll_hospi, 1), 0),
                          "hospitalisations, moyenne mobile 7 jours")) +
    theme_bw() +
    theme(plot.title = element_markdown(face = "bold", size = 9),
          plot.title.position = 'plot',
          plot.subtitle = element_text(size = 8),
          axis.text.x = element_text(size = 7),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

hospi7_line <- hospi_i %>%
    filter(date >= (max(date) - ddays(42))) %>%
    ggplot(aes(date, hospi)) +
    geom_bar(stat = "identity", fill = paletteer_d("nord::polarnight")[4]) +
    geom_line(aes(date, roll_hospi, group = 1, lty = "Moyenne mobile 7 jours"),
              colour = paletteer_d("nord::frost")[4], size = 1.1) +
    scale_x_date(date_breaks = "weeks",
                 labels = date_format("%d%h")) +
    labs(caption = "Denis Haine; source: INSPQ") +
    theme_bw() +
    theme(plot.title = element_markdown(face = 'bold', size = 9),
          plot.title.position = 'plot',
          plot.subtitle = element_text(size = 8),
          plot.caption = element_markdown(margin = margin(t = 15),
                                          size = 5),
          plot.caption.position = 'plot',
          axis.text.x = element_text(size = 7),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

hospi_plot <- hospi_line + hospi7_line +
    plot_layout(guides = 'collect') &
    theme(legend.title = element_blank(),
          legend.position = "bottom")
ggsave(plot = hospi_plot,
       filename = "hospi_plot.png",
       width = 12.944, height = 4, units = "in",
       path = "./static/homepage",
       dpi = "retina")


## Line plot incidence hospi SI
hospiInc_line <- hospi_i %>%
    ggplot(aes(date, si)) +
    geom_bar(stat = "identity", fill = paletteer_d("nord::polarnight")[4]) +
    geom_line(aes(date, roll_si, group = 1, lty = "Moyenne mobile 7 jours"),
              colour = paletteer_d("nord::frost")[4], size = 1.1) +
    scale_x_date(date_breaks = "months",
                 labels = date_format("%b")) +
    labs(title = "Québec, nouvelles admissions aux soins intensifs",
         subtitle = paste(date_report, ":", round(tail(hospi_i$roll_si, 1), 0),
                          "hospitalisations, moyenne mobile 7 jours")) +
    theme_bw() +
    theme(plot.title = element_markdown(face = 'bold', size = 9),
          plot.title.position = 'plot',
          plot.subtitle = element_text(size = 8),
          axis.text.x = element_text(size = 7),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

hospiInc7_line <- hospi_i %>%
    filter(date >= (max(date) - ddays(42))) %>%
    ggplot(aes(date, si)) +
    geom_bar(stat = "identity", fill = paletteer_d("nord::polarnight")[4]) +
    geom_line(aes(date, roll_si, group = 1, lty = "Moyenne mobile 7 jours"),
              colour = paletteer_d("nord::frost")[4], size = 1.1) +
    scale_x_date(date_breaks = "weeks",
                 labels = date_format("%d%h")) +
    labs(caption = "Denis Haine; source: INSPQ") +
    theme_bw() +
    theme(plot.title = element_markdown(face = 'bold', size = 9),
          plot.title.position = 'plot',
          plot.subtitle = element_text(size = 8),
          plot.caption = element_markdown(margin = margin(t = 15),
                                          size = 5),
          plot.caption.position = 'plot',
          axis.text.x = element_text(size = 7),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())

hospiInc_plot <- hospiInc_line + hospiInc7_line +
    plot_layout(guides = 'collect') &
    theme(legend.title = element_blank(),
          legend.position = "bottom")
ggsave(plot = hospiInc_plot,
       filename = "hospiInc_plot.png",
       width = 12.944, height = 4, units = "in",
       path = "./static/homepage",
       dpi = "retina")

date_report <- format(max(hospi_p$date), '%d%h%Y')
hospi_prev <- hospi_pl %>%
    ggplot(aes(date, nbr, fill = si_ornot)) +
    geom_area(alpha = 0.75) +
    scale_fill_paletteer_d("nord::aurora") +
    scale_x_date(date_breaks = "months",
                 labels = date_format("%b")) +
    labs(title = "Hospitalisations et soins intensifs au Québec",
         subtitle = date_report,
         caption = "Denis Haine; source: INSPQ") +
    theme_bw() +
    theme(plot.title = element_markdown(face = 'bold', size = 9),
          plot.title.position = 'plot',
          plot.subtitle = element_text(size = 8),
          plot.caption = element_markdown(margin = margin(t = 15),
                                          size = 5),
          plot.caption.position = 'plot',
          axis.text.x = element_text(size = 7),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.5, .75),
          legend.box.background = element_rect(fill = "transparent",
                                               colour = "transparent"))
ggsave(plot = hospi_prev,
       filename = "hospi_prev.png",
       width = 6.472, height = 4, units = "in",
       path = "./static/homepage",
       dpi = "retina")

date_report <- format(max(hospi_al$date), '%d%h%Y')
hospi_al %>% group_by(date) %>% arrange(age, si_ornot, .by_group = TRUE) %>%
    filter(age == "50-59 ans") %>%
    ggplot(aes(date, nbr, fill = si_ornot)) +
    geom_area(alpha = 0.75) +
    scale_fill_paletteer_d("nord::aurora") +
    scale_x_date(date_breaks = "months",
                 labels = date_format("%b")) +
#    facet_wrap(vars(age), ncol = 5, scales = "free") +
    labs(title = "Hospitalisations et soins intensifs au Québec, par classe d'âge",
         subtitle = date_report,
         caption = "Denis Haine; source: INSPQ") +
    theme_bw() +
    theme(plot.title = element_markdown(face = 'bold', size = 9),
          plot.title.position = 'plot',
          plot.subtitle = element_text(size = 8),
          plot.caption = element_markdown(margin = margin(t = 15),
                                          size = 5),
          plot.caption.position = 'plot',
          axis.text.x = element_text(size = 7),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.box.background = element_rect(fill = "transparent",
                                               colour = "transparent"))

#hospi_prop <- hospi_al %>% group_by(date, si_ornot) %>%
#    summarise(nbr = nbr) %>%
#    mutate(prop = nbr/sum(nbr))
#hospi_tot <- hospi_al %>% group_by(date, si_ornot) %>%
#    summarise(tot = sum(nbr))
#%>%
#    mutate(prop = nbr/sum(nbr))
